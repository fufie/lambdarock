#include "ecl/ecl.h"

int main(int argc, char **argv) {

    cl_boot(argc, argv);
    cl_object obj=c_string_to_object("\"game.lisp\"");
    cl_load(1,obj);
    cl_object obj2=c_string_to_object("(lb::c)");
    cl_eval(obj2);
    cl_shutdown();
}
