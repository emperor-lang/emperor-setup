#include "emperor-setup-args.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * @brief Entry-point for emperor-setup.
 *
 * Calls argument parser then gives _appropriate_ output.
 *
 * @param argc	The number of command-line arguments
 * @param argv	The contents of the command-lint arguments
 * @return int
 */
int main(int argc, char** argv);

/**
 * @brief 
 * 
 * @return const char* 
 */
const char* cFlags();
const char* libs();
const char* binaryInstallLocation();
const char* libraryInstallLocation();
const char* dataInstallLocation();
