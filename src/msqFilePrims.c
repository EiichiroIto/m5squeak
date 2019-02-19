#include "msq.h"

int sqFileAtEnd(SQFile *f)
{
  success(false);
}

int sqFileClose(SQFile *f)
{
  success(false);
}

int sqFileDeleteNameSize(int sqFileNameIndex, int sqFileNameSize)
{
  success(false);
}

int sqFileGetPosition(SQFile *f)
{
  success(false);
}

int sqFileInit(void)
{
  /* noop */
}

int sqFileOpen(SQFile *f, int sqFileNameIndex, int sqFileNameSize, int writeFlag)
{
  success(false);
}

int sqFileReadIntoAt(SQFile *f, int count, int byteArrayIndex, int startIndex)
{
  success(false);
}

int sqFileRenameOldSizeNewSize(int oldNameIndex, int oldNameSize, int newNameIndex, int newNameSize)
{
  success(false);
}

int sqFileSetPosition(SQFile *f, int position)
{
  success(false);
}

int sqFileSize(SQFile *f)
{
  success(false);
}

int sqFileValid(SQFile *f)
{
  return 0;
}

int sqFileWriteFromAt(SQFile *f, int count, int byteArrayIndex, int startIndex)
{
  success(false);
}

int dir_Create(char *pathString, int pathStringLength)
{
  success(false);
}

int dir_Delimitor(void)
{
  return '/';
}

int dir_Lookup(char *pathString, int pathStringLength, int index, char *name, int *nameLength, int *creationDate, int *modificationDate, int *isDirectory, int *sizeIfFile)
{
  success(false);
}

int dir_SetMacFileTypeAndCreator(char *filename, int filenameSize, char *fType, char *fCreator)
{
  /* noop */
}