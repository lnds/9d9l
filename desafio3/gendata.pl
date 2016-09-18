#!/bin/perl

# Generate data for challenge 3
# Genra los datos para el desafio 3

my $limit = shift or die "uso: gendata.pl limite\n";

for (my $id = 0; $id < $limit; $id++) {

	printf "%09d", $id;

	for (my $i = 0; $i < 6; $i++) {
		my $ri = rand(200);
		if ($ri < 180) {
			printf("%s", "000000"x23);
		} 
		else {
			for (my $p = 0; $p < 23; $p++) {
				my $y = rand(6);
				if ($y > 3) {
					$y = $y + 2000;
					$m = rand(12)+1;
				} else {
					$y = 0;
					$m = 0;
				}
				printf("%04d%02d", $y, $m);
			}
		}
	}
	print "\n";
}

