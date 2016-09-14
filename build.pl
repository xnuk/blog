foreach (grep {$_ =~ /\.md$/} map {chomp; $_} `find posts/`){
	my $src = $_;
	s/^posts/dest/;
	s/\.md$/\.html/;
	my $dest = $_;
	s/\/[^\/]*$/\//;
	`mkdir -p $_`;
	`pandoc --template src/template.html --mathml $src -o $dest`
}
