foreach (grep {$_ =~ /\.md$/} map {chomp; $_} `find posts/`){
	/^posts\/(((?:.+)(?:\/.+)*)\/)?([^\/]+)\.md$/;
	my $a = "--title-prefix " . $2 if $2;
	`mkdir -p dest/$1; pandoc --variable "root:/blog/" --template src/template.html $a --mathml $& -o dest/$1$3.html\n`
}
