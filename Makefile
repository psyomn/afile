all: debug

debug:
	gnatmake -P afile -Xmode=debug -p

release:
	gnatmake -P afile -Xmode=release -p
