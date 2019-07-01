#include "emperor-setup.h"

static const char tooFewArgsHelp[] = "incorrect usage\nTry emperor-setup --help for more information";

// TODO: run argument parser generator for options

int main(int argc, char **argv)
{
	// Check sufficient arguments have been given
	if (argc <= 1)
	{
		fprintf(stderr, "%s: %s\n", argv[0], tooFewArgsHelp);
		exit(-1);
	}

	// Output gcc flags for each mapping
	for (int i = 1; i < argc; i++)
	{
		if (strcmp("-h", argv[i]) == 0 || strcmp("--help", argv[i]) == 0)
		{
			printf("%s\n", help());
		}
		else if (strcmp("--cflags", argv[i]) == 0)
		{
			printf("%s\n", cFlags());
		}
		else if (strcmp("--clibs", argv[i]) == 0)
		{
			printf("%s\n", cLibs());
		}
		else
		{
			fprintf(stderr, "%s %s\n", "Unrecognised argument:", argv[i]);
			exit(-1);
		}
	}

	return 0;
}

const char *help()
{
	static const char help[] = "Help message";
	return help;
}

const char *cFlags()
{
	// -std=c18
	const char *cFlags = "-Wall -Werror -Wpedantic -pedantic-errors -I . -I /usr/include/emperor/ -O3 -g";
	return cFlags;
}

const char *cLibs()
{
	static const char cLibs[] = "-I . -I /usr/include/emperor/ -l pcre";
	return cLibs;
}