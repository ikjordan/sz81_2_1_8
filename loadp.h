#ifndef _LOADP_H_
#define _LOADP_H_

bool loadPInitialise(char* fullpath, int filename, bool zx80);
void loadPUninitialise(void);
int loadPGetBit(void);

#endif // _LOADP_H_
