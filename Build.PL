use Module::Build;

use 5.6.0;

use strict;
use warnings;

my $build = Module::Build->new(
    module_name => 'Tree::Compat',
    license => 'perl',
    requires => {
        'perl'               => '5.6.0',
        'Tree'               => '1.00',
        'UNIVERSAL::require' => '0.10',
    },
    build_requires => {
        'Test::More'           => '0.47',
    },
    create_makefile_pl => 'traditional',
    recursive_test_files => 1,
    add_to_cleanup => [
        'META.yml', '*.bak', '*.gz', 'Makefile.PL',
    ],
);

$build->create_build_script;
