/* Embeded Image File.  Author: Eiichiro.Ito@gmail.com
 *
 * Last edited: Thu Feb 14 20:44:10 2019 by EiichiroIto (Eiichiro Ito) on xps13
 */

#include "msq.h"

static int sqImageFilePos = 0;
extern const unsigned char squeak_image[];

void sqImageFileClose(int f) {
  
}

int sqImageFileOpen(char *fileName, char *mode) {
  sqImageFilePos = 0;
  return (int) 1;
}

int sqImageFilePosition(int f) {
  return sqImageFilePos;
}

int sqImageFileRead(void *ptr, int elementSize, int count, int f) {
  unsigned char *dst = (unsigned char *) ptr;
  const unsigned char *src = &squeak_image[sqImageFilePos] ;
  int size = elementSize * count;

  if (size == 0) {
    return 0;
  }
  sqImageFilePos += size;
  while (size --) {
    *dst++ = *src++;
  }
  return count;
}

void sqImageFileSeek(int f, int pos) {
  sqImageFilePos = pos;
}
