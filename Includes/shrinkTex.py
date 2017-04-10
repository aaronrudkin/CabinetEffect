file_set = ["table1.tex", "table2.tex"]

for file_name in file_set:
	dat = open(file_name, "r").read()
	if not r"\scalebox" in dat:
		dat = dat.replace(r"\begin{tabular}",r"\scalebox{0.9}{\begin{tabular}")
		dat = dat.replace(r"\end{tabular}",r"\end{tabular}}")
		print dat
		with open(file_name, "w") as f:
			f.write(dat)
	else:
		print "already ok"