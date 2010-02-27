use strict;
use warnings;
use inc::Module::Install;

my $dist = 'POE-Component-IRC-Plugin-ImageMirror';
name           ($dist);
author         ('Hinrik Örn Sigurðsson <hinrik.sig@gmail.com>');
abstract_from  ('lib/POE/Component/IRC/Plugin/ImageMirror.pm');
version_from   ('lib/POE/Component/IRC/Plugin/ImageMirror.pm');
license        ('perl');
homepage       ("http://search.cpan.org/dist/$dist");
githubmeta     ();
bugtracker     ("http://github.com/hinrik/\L$dist/issues");
resources      (Ratings => "http://cpanratings.perl.org/d/$dist");

requires       ('HTTP::Cookies'       => '0');
requires       ('HTTP::Headers'       => '0');
requires       ('List::MoreUtils'     => '0');
requires       ('LWP::UserAgent::POE' => '0');
requires       ('POE'                 => '0');
requires       ('POE::Wheel::Run'     => '0');
requires       ('POE::Component::IRC' => '6.22');
requires       ('POE::Component::IRC::Plugin::URI::Find' => '0');
requires       ('URI::Title'          => '0');

test_requires  ('Test::More' => '0');
tests_recursive();

author_requires('Perl::Critic'        => '1.098');
author_requires('Test::Perl::Critic'  => '0');
author_requires('Test::Pod'           => '1.41');
author_requires('Test::Pod::Coverage' => '1.08');
author_tests   ('xt');

WriteAll();
