#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "conversion.h"


short c_floatToString(double f,char* s)
{
    sprintf(s,"%.5f",f);
    return(strlen(s));
}

short c_shortToString(short d,char* s)
{
    sprintf(s,"%d",d);
    return(strlen(s));
}