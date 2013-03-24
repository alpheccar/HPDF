#include "metrics.h"
#include "ctext.h"
#include <stdio.h>

#define GFONTSIZE (255-32+1)
#define FFONTSIZE (5)

short c_getAdvance(unsigned short font,short c)
{
    if (c<32)
        return(0);
    if (c-32 >= GFONTSIZE)
        return(0);
    return(gmetric[font*GFONTSIZE+c-32]);
}

short c_getLeading(unsigned short font)
{
    return(fmetric[font*FFONTSIZE+2]-fmetric[font*FFONTSIZE+3]);
}

short c_getDescent(unsigned short font)
{
    return(-fmetric[font*FFONTSIZE+3]);
}

int c_hasKern(unsigned short font)
{
    return(fmetric[font*FFONTSIZE+4]);
}
