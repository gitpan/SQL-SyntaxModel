use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
	NAME	=> 'SQL::SyntaxModel',
	VERSION_FROM => 'lib/SQL/SyntaxModel.pm', # finds $VERSION
	PREREQ_PM => {
		'Locale::KeyedText' => 0.07,
	},
);
