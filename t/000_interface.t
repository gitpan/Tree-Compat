use strict;
use warnings;

use Test::More tests => 2;

use_ok( 'Tree::Compat' );

can_ok( 'Tree::Compat', 'load' );
