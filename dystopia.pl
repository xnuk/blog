# Don't try this at home
if ($ENV{'CIRCLECI'}) {
	foreach(grep {$_ !~ /^(\.+(git)?|dest)$/} <.* *>) { `rm -rf $_` }
}
