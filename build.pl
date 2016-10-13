foreach (grep {$_ =~ /\.(?:png|jpg|gif)$/} map {chomp; $_} `find posts/`){
	m|^posts/(.+/)?([^/]+)$|;
	`mkdir -p dest/$1; cp posts/$1$2 dest/$1;`
}
