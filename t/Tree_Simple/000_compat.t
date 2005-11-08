use strict;
use warnings;

use Test::More tests => 4;

use_ok( 'Tree::Compat', 'Tree::Simple' );

my $tree = Tree::Simple->new;
isa_ok( $tree, 'Tree::Simple' );

my $real_tree = $tree->REAL_TREE;
isnt( $real_tree, $tree );
is( $real_tree, ${$tree} );
