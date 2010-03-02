package POE::Component::IRC::Plugin::ImageMirror;

use strict;
use warnings;
use HTTP::Cookies;
use HTTP::Headers;
use List::MoreUtils qw(natatime);
use LWP::UserAgent::POE;
use POE;
use POE::Component::IRC::Plugin qw(PCI_EAT_NONE PCI_EAT_PLUGIN);
use POE::Component::IRC::Plugin::URI::Find;
use POE::Wheel::Run;
use URI::Title qw(title);

our $VERSION = '0.06';

sub new {
    my ($package, %args) = @_;
    my $self = bless \%args, $package;

    # defaults
    $self->{useragent} =
      'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9b3pre) Gecko/2008020108'
      if !defined $self->{useragent};
    $self->{URI_match} = [qr/(?i:jpe?g|gif|png)$/] if !$self->{URI_match};
    $self->{URI_title} = 1 if !defined $self->{URI_title};
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
                _no_uri_title
                _mirror_imgur
                _mirror_imgshack
                _post_uri
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

    if (ref $self->{Channels} eq 'ARRAY') {
        my $ok;
        for my $chan (@{ $self->{Channels} }) {
            $ok = 1 if $chan eq $where;
        }
        return PCI_EAT_NONE if !$ok;
    }

    my $matched;
    for my $match (@{ $self->{URI_match} }) {
        $matched = 1 if $uri =~ $match;
        last if $matched;
    }
    return PCI_EAT_NONE if !$matched;

    if (defined $self->{req}{$uri}) {
        return $self->{Eat}
            ? PCI_EAT_PLUGIN
            : PCI_EAT_NONE;
    }

    if ($self->{URI_subst}) {
        my $iter = natatime 2, @{ $self->{URI_subst} };
        while (my ($r, $s) = $iter->()) {
            $uri =~ s/$r/$s/;
        }
    }

    my $sender = POE::Kernel->get_active_session;
    if ($self->{URI_title}) {
        POE::Kernel->post($self->{session_id}, _uri_title => $sender, $where, $uri);
    }
    else {
        POE::Kernel->post($self->{session_id}, _no_uri_title => $sender, $where, $uri);
    }

    return $self->{Eat}
        ? PCI_EAT_PLUGIN
        : PCI_EAT_NONE;
}

sub _no_uri_title {
    my ($kernel, $self, $sender, $where, $uri) = @_[KERNEL, OBJECT, ARG0..ARG2];

    $self->{req}{$uri} = {
        sender   => $sender,
        where    => $where,
        orig_uri => $uri,
    };
    $kernel->yield(_mirror_imgur => $uri);
    $kernel->yield(_mirror_imgshack => $uri);
    $kernel->refcount_increment($sender, __PACKAGE__);
    return;
}

sub _uri_title {
    my ($kernel, $self, $sender, $where, $uri) = @_[KERNEL, OBJECT, ARG0..ARG2];

    my @inc = map { +'-I' => $_ } @INC;
    my $wheel = POE::Wheel::Run->new(
        Program     => sub { title($uri) },
        StdoutEvent => '_child_stdout',
        StderrEvent => '_child_stderr',
        ($^O eq 'MSWin32' ? (CloseOnCall => 0) : (CloseOnCall => 1)),
    );

    $self->{req}{$uri} = {
        sender   => $sender,
        where    => $where,
        wheel    => $wheel,
        orig_uri => $uri,
    };

    $kernel->sig_child($wheel->PID, '_sig_chld');
    $kernel->refcount_increment($sender, __PACKAGE__);
    return;
}

sub _sig_chld {
    $_[KERNEL]->sig_handled;
    return;
}

sub _child_stderr {
    my ($kernel, $self, $input) = @_[KERNEL, OBJECT, ARG0];
    warn "$input\n" if $self->{debug};
    return;
}

sub _child_stdout {
    my ($kernel, $self, $title, $id) = @_[KERNEL, OBJECT, ARG0, ARG1];

    my $uri;
    for my $key (keys %{ $self->{req} }) {
        if ($self->{req}{$key}{wheel}->ID eq $id) {
            $uri = $key;
            last;
        }
    }

    $self->{req}{$uri}{title} = $title;

    $kernel->yield(_mirror_imgur => $uri);
    $kernel->yield(_mirror_imgshack => $uri);
    return;
}

sub _mirror_imgur {
    my ($kernel, $self, $orig_uri) = @_[KERNEL, OBJECT, ARG0];

    my $ua = LWP::UserAgent::POE->new(
        cookie_jar            => HTTP::Cookies->new,
        requests_redirectable => [qw(GET HEAD POST)],
    );

    if (defined $self->{Imgur_user} && defined $self->{Imgur_pass}) {
        $ua->post(
            'http://imgur.com/signin',
            {
                username => $self->{Imgur_user},
                password => $self->{Imgur_pass},
                submit   => '',
            },
        );
    }

    my $res = $ua->get("http://imgur.com/api/upload/?url=$orig_uri");

    my $imgur;
    if ($res->is_success) {
        if (my ($uri) = $res->content =~ m{id="direct"\s+value="(.*?)"}) {
            $imgur = $uri;
        }
    }

    $self->{req}{$orig_uri}{imgur_uri} = defined $imgur ? $imgur : '';

    # post the url if we've got both now
    if (defined $self->{req}{$orig_uri}{imgshack_uri}) {
        $kernel->yield(_post_uri => $orig_uri);
    }
    return;
}

sub _mirror_imgshack {
    my ($kernel, $self, $orig_uri) = @_[KERNEL, OBJECT, ARG0];

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

    my $imgshack;
    if ($res->is_success) {
        if (my ($uri) = $res->content =~ m{<a.*? href="(.*?)"[^>]+>Direct}) {
            $imgshack = $uri;
        }
    }

    $self->{req}{$orig_uri}{imgshack_uri} = defined $imgshack ? $imgshack : '';

    # post the url if we've got both now
    if (defined $self->{req}{$orig_uri}{imgur_uri}) {
        $kernel->yield(_post_uri => $orig_uri);
    }
    return;
}

sub _post_uri {
    my ($kernel, $self, $uri) = @_[KERNEL, OBJECT, ARG0];

    my $req = delete $self->{req}{$uri};
    my $title = $self->{URI_title} ? "$req->{title} - " : '';
    $self->{irc}->yield(
        $self->{Method},
        $req->{where},
        "$title$req->{imgur_uri} / $req->{imgshack_uri}",
    );

    $kernel->refcount_decrement($req->{sender}, __PACKAGE__);
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
the channel log and uploads the images to Imageshack and Imgur, then prints a
short description of the image along with the new URLs.

 <avar> http://images.4chan.org/b/src/1267339589262.gif
 -MyBot:#channel- gif (318 x 241) - http://imgur.com/RWcSE.gif - http://img535.imageshack.us/img535/9685/1267339589262.gif

This plugin makes use of
L<POE::Component::IRC::Plugin::URI::Find|POE::Component::IRC::URI::Find>. An
instance will be added to the plugin pipeline if it is not already present.

=head1 METHODS

=head2 C<new>

Takes the following optional arguments:

B<'Channels'>, an array reference of channels names. If you don't supply
this, images will be mirrored in all channels.

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

B<'URI_title'>, whether or not to include a title produced by
L<URI::Title|URI::Title>. Defaults to true.

B<'Imgur_user'>, an Imgur username. If provided, the images uploaded to Imgur
will be under this account rather than anonymous.

B<'Imgur_pass'>, an Imgur account password to go with B<'ImgurUser'>.

B<'Method'>, how you want messages to be delivered. Valid options are
'notice' (the default) and 'privmsg'.

B<'Eat'>, when enabled, will prevent further processing of C<irc_urifind_uri>
events by other plugins for URIs which this plugin mirrors. False by default.

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
