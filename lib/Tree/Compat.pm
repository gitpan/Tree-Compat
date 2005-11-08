package Tree::Compat;

use strict;
use warnings;

use UNIVERSAL::require;

our $VERSION = '1.00';

sub import {
    my $self = shift;
    foreach my $module ( @_ ) {
        $self->load( $module );
    }
}

my %mapping = (
    'Tree::Simple'   => 'Tree::Compat::Tree::Simple',
    'Tree::DAG_Node' => 'Tree::Compat::Tree::DAG_Node',
    'Tree::Nary'     => 'Tree::Compat::Tree::Nary',
);

sub load {
    my $self = shift;
    my $class = shift;

    unless ( exists $mapping{ $class } ) {
        die "'$class' doesn't have a compatibility layer written yet.\n";
    }

    $mapping{ $class }->require or die $UNIVERSAL::require::ERROR;
}

1;
__END__

=head1 NAME

Tree::Compat - a compatibility layer for Tree

=head1 SYNOPSIS

  use Tree::Compat 'Tree::Simple';

  # or ...
  
  use Tree::Compat;

  Tree::Compat->load( 'Tree::Simple' );

=head1 DESCRIPTION

This is meant to be a compatibility layer to aid in transition from various
other Tree::* modules to L<Tree>. As part of the transition, the REAL_TREE()
method is provided in all the compatibility layers in order to access the
L<Tree> object underneath.

=head1 METHODS

=head2 B<load( $pkg_name )>

This will load the compatibility layer for C<$pkg_name>. If one hasn't been
written yet, then load() will die.

=head1 COMPATIBILITY LAYERS

Compats have been written for:

=over 4

=item *

L<Tree::DAG_Node>

=item *

L<Tree::Nary>

=item *

L<Tree::Simple>

=back

=head1 LIMITATIONS

These compatibility layers provide compatibility with the published API of the
specific module. If your code uses anything that begins with an underscore or
directly accesses the internal state of the object, B<YOUR CODE WILL BREAK>,
and it will probably break at run-time.

In addition, the compatibility layer will most likely be slower than the
original code. There's at least one (and often more) layer(s) of indirection
involved in a true compatibility layer, especially between two modules with
differing design philosophies.

=head1 CODE COVERAGE

We use L<Devel::Cover> to test the code coverage of our tests. Below is the
L<Devel::Cover> report on this module's test suite.

  ---------------------------- ------ ------ ------ ------ ------ ------ ------
  File                           stmt   bran   cond    sub    pod   time  total
  ---------------------------- ------ ------ ------ ------ ------ ------ ------
  blib/lib/Tree/Compat.pm        94.1   50.0    n/a  100.0  100.0    0.5   88.9
  ...ree/Compat/Tree/Simple.pm   96.8   94.2  100.0   89.7   93.3   99.5   95.3
  Total                          96.6   92.2  100.0   90.9   93.5  100.0   94.9
  ---------------------------- ------ ------ ------ ------ ------ ------ ------

=head1 SUPPORT

The mailing list is at L<TreeCPAN@googlegroups.com>. I also read
L<http://www.perlmonks.com> on a daily basis.

=head1 AUTHORS

Rob Kinyon E<lt>rob.kinyon@iinteractive.comE<gt>

Stevan Little E<lt>stevan.little@iinteractive.comE<gt>

Thanks to Infinity Interactive for generously donating our time.

=head1 COPYRIGHT AND LICENSE

Copyright 2004, 2005 by Infinity Interactive, Inc.

L<http://www.iinteractive.com>

This library is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. 

=cut
