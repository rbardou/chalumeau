default:
	@ocaml make.ml

clean:
	rm -rf bin chalumeau

.PHONY: default clean
