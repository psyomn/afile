ADA=gprbuild
all: debug

debug:
	$(ADA) -P afile -Xmode=debug -p

release:
	$(ADA) -P afile -Xmode=release -p
