package POE::Component::IRC::Plugin::ImageMirror;

use strict;
use warnings;
use HTTP::Cookies;
use HTTP::Headers;
use List::MoreUtils qw(natatime);
use LWP::UserAgent::POE;
use POE;
use POE::Component::IRC::Plugin qw(PCI_EAT_NONE);
use POE::Component::IRC::Plugin::URI::Find;
use POE::Wheel::Run;

our $VERSION = '0.02';

my $uri_title_code = <<'END';
use strict;
use warnings;
use URI::Title qw(title);
$| = 1;
print title($ARGV[0]), "\n";
END

sub new {
    my ($package, %args) = @_;
    my $self = bless \%args, $package;

    # defaults
    $self->{useragent} =
      'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9b3pre) Gecko/2008020108'
      if !defined $self->{useragent};
    $self->{URI_match} = [qr/(?i:jpe?g|gif|png)$/] if !$self->{URI_match};
    $self->{Method} = 'notice' if !defined $self->{Method};

    return $self;
}

sub PCI_register {
    my ($self, $irc) = @_;

    if ( !grep { $_->isa('POE::Component::IRC::Plugin::URI::Find') } values %{ $irc->plugin_list() } ) {
        $irc->plugin_add('URIFind', POE::Component::IRC::Plugin::URI::Find->new());
    }

    $self->{irc} = $irc;
    POE::Session->create(
        object_states => [
            $self => [qw(
                _start
                _sig_DIE
                _sig_chld
                _child_stdout
                _child_stderr
                _uri_title
                _mirror_uri
            )],
        ],
    );

    $irc->plugin_register($self, 'SERVER', qw(urifind_uri));
    return 1;
}

sub PCI_unregister {
    my ($self, $irc) = @_;
    $poe_kernel->refcount_decrement($self->{session_id}, __PACKAGE__);
    return 1;
}

sub _start {
    my ($kernel, $self, $session) = @_[KERNEL, OBJECT, SESSION];
    $self->{session_id} = $session->ID();
    $kernel->sig(DIE => '_sig_DIE');
    $kernel->refcount_increment($self->{session_id}, __PACKAGE__);
    return;
}

sub _sig_DIE {
    my ($kernel, $self, $ex) = @_[KERNEL, OBJECT, ARG1];
    chomp $ex->{error_str};
    warn "Error: Event $ex->{event} in $ex->{dest_session} raised exception:\n";
    warn "  $ex->{error_str}\n";
    $kernel->sig_handled();
    return;
}

sub S_urifind_uri {
    my ($self, $irc) = splice @_, 0, 2;
    my $where = ${ $_[1] };
    my $uri   = ${ $_[2] };

    my $matched;
    for my $match (@{ $self->{URI_match} }) {
        $matched = 1 if $uri =~ $match;
        last if $matched;
    }
    return PCI_EAT_NONE if !$matched;

    
    if ($self->{URI_subst}) {
        my $iter = natatime 2, @{ $self->{URI_subst} };
        while (my ($r, $s) = $iter->()) {
            $uri =~ s/$r/$s/;
        }
    }

    my $sender = POE::Kernel->get_active_session;
    POE::Kernel->post($self->{session_id}, _uri_title => $sender, $where, $uri);
    return PCI_EAT_NONE;
}

sub _uri_title {
    my ($kernel, $self, $sender, $where, $uri) = @_[KERNEL, OBJECT, ARG0..ARG2];

    my @inc = map { +'-I' => $_ } @INC;
    my $wheel = POE::Wheel::Run->new(
        Program     => [$^X, @inc, '-e', $uri_title_code, $uri],
        StdoutEvent => '_child_stdout',
        StderrEvent => '_child_stderr',
    );

    $self->{req}{ $wheel->ID } = [$sender, $where, $uri, $wheel];
    $kernel->sig_child($wheel->PID, '_sig_chld');
    $kernel->refcount_increment($sender, __PACKAGE__);
    return;
}

sub _child_stdout {
    my ($kernel, $self, $title, $id) = @_[KERNEL, OBJECT, ARG0, ARG1];
    my ($sender, $where, $uri, $wheel) = @{ delete $self->{req}{$id} };
    $kernel->yield(_mirror_uri => $sender, $where, $uri, $title);
    return;
}

sub _child_stderr {
    my ($kernel, $self, $input) = @_[KERNEL, OBJECT, ARG0];
    warn "$input\n" if $self->{debug};
    return;
}

sub _sig_chld {
    $_[KERNEL]->sig_handled;
    return;
}

sub _mirror_uri {
    my ($kernel, $self, $sender, $where, $orig_uri, $title)
        = @_[KERNEL, OBJECT, ARG0..ARG3];

    my $ua = LWP::UserAgent::POE->new(
        cookie_jar            => HTTP::Cookies->new,
        requests_redirectable => [qw(GET HEAD POST)],
        ua                    => $self->{useragent},
        default_header        => HTTP::Headers->new(
            Referer => 'http://imageshack.us/',
        ),
    );

    my $res = $ua->post(
         'http://www.imageshack.us/transload.php',
         Content_Type => 'multipart/form-data',
         Content      => [
            uploadtype    => 'on',
            url           => $orig_uri,
            email         => '', 
            MAX_FILE_SIZE => 13145728,
            refer         => '', 
            brand         => '', 
            optsize       => 'resample',
        ],
    );
    return if !$res->is_success;

    my $content = $res->content;
    if (my ($uri) = $content =~ m{<a.*? href="(.*?)"[^>]+>Direct}) {
        $uri =~ s/' .*$//;
        $self->{irc}->yield($self->{Method}, $where, "$title - $uri");
    }
    
    $kernel->refcount_decrement($sender, __PACKAGE__);
    return;
}

1;

=encoding utf8

=head1 NAME

POE::Component::IRC::Plugin::ImageMirror - A PoCo-IRC plugin which uploads
select images to a mirror service

=head1 SYNOPSIS

 use POE::Component::IRC::Plugin::ImageMirror;

 # mirror all images from 4chan.org
 $irc->plugin_add(ImageMirror => POE::Component::IRC::Plugin::ImageMirror->new(
     URI_match => [
         qr{4chan\.org/\w+/src/.*(?i:jpe?g|gif|png)$},
     ],
 ));

=head1 DESCRIPTION

POE::Component::IRC::Plugin::ImageMirror is a
L<POE::Component::IRC|POE::Component::IRC> plugin. It looks for image URLs in
the channel log and uploads the images to Imageshack, then prints a
short description of the image along with the new URL.

=head1 METHODS

=head2 C<new>

Takes the following optional arguments:

B<'URI_match'>, an array reference of regex objects. Any url found must match
at least one of these regexes if it is to be uploaded. If you don't supply
this parameter, a default regex of C<qr/(?i:jpe?g|gif|png)$/> is used.

B<'URI_subst'>, an array of regex/string pairs. These substitutions will be
done on the accepted URIs before they are processed further.

Example:

 # always fetch 7chan images via http, not https
 URI_subst => [
     qr{(?<=^)https(?=://(?:www\.)?7chan\.org)} => 'http',
 ]

B<'Method'>, how you want messages to be delivered. Valid options are
'notice' (the default) and 'privmsg'.

Returns a plugin object suitable for feeding to
L<POE::Component::IRC|POE::Component::IRC>'s C<plugin_add> method.

=head1 AUTHOR

Hinrik E<Ouml>rn SigurE<eth>sson, hinrik.sig@gmail.com

Imageshack-related code provided by E<AElig>var ArnfjE<ouml>rE<eth>
Bjarmason <avar@cpan.org>.

=head1 LICENSE AND COPYRIGHT

Copyright 2010 Hinrik E<Ouml>rn SigurE<eth>sson

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
