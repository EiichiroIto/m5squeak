/* Embeded Image File.  Author: Eiichiro.Ito@gmail.com
 *
 * Last edited: Thu Feb 14 20:44:10 2019 by EiichiroIto (Eiichiro Ito) on xps13
 */

#include <M5Core2.h>
#include "msq.h"

static File file;

void sqImageFileClose(int f)
{
  if (file) {
    file.close();
  }
}

int sqImageFileOpen(char *fileName, char *mode)
{
  sqImageFileClose(0);
  if (!SD.exists(fileName)) {
    return 0;
  }
  file = SD.open(fileName, FILE_READ);
  return file ? 1 : 0;
}

int sqImageFilePosition(int f)
{
  return file.position();
}

int sqImageFileRead(void *ptr, int elementSize, int count, int f)
{
  return file.read((uint8_t*) ptr, elementSize * count);
}

void sqImageFileSeek(int f, int pos)
{
  file.seek(pos);
}
