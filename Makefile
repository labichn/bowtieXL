.PHONY: clean bowtieXL

default: clean bowtieXL

bowtieXL: clean
	cd code && ocamlbuild -use-ocamlfind bowtieXL.native
	cp code/bowtieXL.native bowtieXL

clean:
	cd code && ocamlbuild -clean
	rm -f code/gmon.out gmon.out bowtieXL
