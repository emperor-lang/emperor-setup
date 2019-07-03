#include "emperor-setup.h"
#include "emperor-setup-args.h"

int main(int argc, char** argv)
{
	args_t* args = parseArgs(argc, argv);

	// Ensure the correct number of arguments
	if (args->cFlags & args->libs)
	{
		fprintf(stderr, "Please use one flag per call\n");
		free(args);
		exit(1);
	}

	// Output the correct set of flags
	if (args->cFlags)
	{
		printf("%s\n", cFlags());
	}
	else if (args->libs)
	{
		printf("%s\n", libs());
	}

	free(args);

	return 0;
}

const char* help()
{
	static const char help[] = "Help message";
	return help;
}

const char* cFlags()
{
	// -std=c18
	const char* cFlags = "-Wall -Werror -Wpedantic -pedantic-errors -I . -I /usr/include/emperor/ -O3 -g";
	return cFlags;
}

const char* libs()
{
	static const char libs[] = "-l pcre";
	return libs;
}