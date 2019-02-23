/*#include "sqConfig.h"*/

#define true 1
#define false 0
#define null 0  /* using 'null' because nil is predefined in Think C */

/* pluggable primitives macros */
/* Note: All pluggable primitives are defined as
	EXPORT(int) somePrimitive(void)
   If the platform requires special declaration modifiers
   the EXPORT macro can be redefined
*/
#define EXPORT(returnType) returnType

/* image save/restore macros */
/* Note: The image file save and restore code uses these macros; they
   can be redefined in sqPlatformSpecific.h if desired. These default
   versions are defined in terms of the ANSI Standard C libraries.
*/
#define sqAllocateMemory(minHeapSize, desiredHeapSize)   ps_malloc(desiredHeapSize)

/* platform-dependent float conversion macros */
/* Note: Second argument must be a variable name, not an expression! */
/* Note: Floats in image are always in PowerPC word order; change
   these macros to swap words if necessary. This costs no extra and
   obviates sometimes having to word-swap floats when reading an image.
*/
#if defined(DOUBLE_WORD_ALIGNMENT) || defined(DOUBLE_WORD_ORDER)
# ifdef DOUBLE_WORD_ORDER
/* word-based copy with swapping for non-PowerPC order */
#   define storeFloatAtfrom(i, floatVarName) \
	*((int *) (i) + 0) = *((int *) &(floatVarName) + 1); \
	*((int *) (i) + 1) = *((int *) &(floatVarName) + 0);
#   define fetchFloatAtinto(i, floatVarName) \
	*((int *) &(floatVarName) + 0) = *((int *) (i) + 1); \
	*((int *) &(floatVarName) + 1) = *((int *) (i) + 0);
# else /*!DOUBLE_WORD_ORDER*/
/* word-based copy for machines with alignment restrictions */
#   define storeFloatAtfrom(i, floatVarName) \
	*((int *) (i) + 0) = *((int *) &(floatVarName) + 0); \
	*((int *) (i) + 1) = *((int *) &(floatVarName) + 1);
#   define fetchFloatAtinto(i, floatVarName) \
	*((int *) &(floatVarName) + 0) = *((int *) (i) + 0); \
	*((int *) &(floatVarName) + 1) = *((int *) (i) + 1);
# endif /*!DOUBLE_WORD_ORDER*/
#else /*!(DOUBLE_WORD_ORDER||DOUBLE_WORD_ALIGNMENT)*/
/* for machines that allow doubles to be on any word boundary */
# define storeFloatAtfrom(i, floatVarName) \
	*((double *) (i)) = (floatVarName);
# define fetchFloatAtinto(i, floatVarName) \
	(floatVarName) = *((double *) (i));
#endif

/* this include file may redefine earlier definitions and macros: */
/*#include "sqPlatformSpecific.h"*/

/*** increment this version number when the image file format changes ***/
#define CURRENT_VERSION 6502

/* squeak file record; see sqFilePrims.c for details */
typedef struct {
	int		sessionID;
	int		writable;
	int		fileSize;
	int		lastOp;  /* 0 = uncommitted, 1 = read, 2 = write */
} SQFile;

#ifdef __cplusplus
extern "C" {
#endif

/* file i/o */
int sqFileAtEnd(SQFile *f);
int sqFileClose(SQFile *f);
int sqFileDeleteNameSize(int sqFileNameIndex, int sqFileNameSize);
int sqFileGetPosition(SQFile *f);
int sqFileInit(void);
int sqFileOpen(SQFile *f, int sqFileNameIndex, int sqFileNameSize, int writeFlag);
int sqFileReadIntoAt(SQFile *f, int count, int byteArrayIndex, int startIndex);
int sqFileRenameOldSizeNewSize(int oldNameIndex, int oldNameSize, int newNameIndex, int newNameSize);
int sqFileSetPosition(SQFile *f, int position);
int sqFileSize(SQFile *f);
int sqFileValid(SQFile *f);
int sqFileWriteFromAt(SQFile *f, int count, int byteArrayIndex, int startIndex);

/* directories */
int dir_Create(char *pathString, int pathStringLength);
int dir_Delimitor(void);
int dir_Lookup(char *pathString, int pathStringLength, int index,
	/* outputs: */
	char *name, int *nameLength, int *creationDate, int *modificationDate,
	int *isDirectory, int *sizeIfFile);
int dir_PathToWorkingDir(char *pathName, int pathNameMax);
int dir_SetMacFileTypeAndCreator(char *filename, int filenameSize, char *fType, char *fCreator);

/* interpreter entry points */
void error(char *s);
int checkedByteAt(int byteAddress);
void checkedByteAtput(int byteAddress, int byte);
int checkedLongAt(int byteAddress);
void checkedLongAtput(int byteAddress, int a32BitInteger);
void fullDisplayUpdate(void);
void initializeInterpreter(int bytesToShift);
void interpret(void);
int primitiveFail(void);
int signalSemaphoreWithIndex(int index);
int success(int);

/* display, mouse, keyboard, time i/o */
void ioBeep(void);
int ioExit(void);
void ioError(const char *str);
void ioForceDisplayUpdate(void);
void ioFormPrint(int bitsAddr, int width, int height, int depth, double hScale, double vScale, int landscapeFlag);
void ioSetFullScreen(int fullScreen);
int ioGetButtonState(void);
int ioGetKeystroke(void);
int ioMicroMSecs(void);
int ioMSecs(void);
int ioLowResMSecs(void);
int ioMousePoint(void);
int ioPeekKeystroke(void);
void ioProcessEvents(void);
void ioPutChar(int);
int ioRelinquishProcessorForMicroseconds(int microSeconds);
int ioScreenSize(void);
int ioSeconds(void);
void ioSetCursor(int cursorBitsIndex, int offsetX, int offsetY);
void ioSetCursorWithMask(int cursorBitsIndex, int cursorMaskIndex, int offsetX, int offsetY);
void ioShowDisplay(int dispBitsIndex, int width, int height, int depth, int affectedL, int affectedR, int affectedT, int affectedB);

/* image file and VM path names */
extern char imageName[];
int imageNameGetLength(int sqImageNameIndex, int length);
int imageNamePutLength(int sqImageNameIndex, int length);
int imageNameSize(void);
int vmPathSize(void);
int vmPathGetLength(int sqVMPathIndex, int length);

/* save/restore */
int readImageFromFileHeapSize(int f, int desiredHeapSize);

/* clipboard (cut/copy/paste) */
int clipboardSize(void);
int clipboardReadIntoAt(int count, int byteArrayIndex, int startIndex);
int clipboardWriteFromAt(int count, int byteArrayIndex, int startIndex);

/* sound output */
int snd_AvailableSpace(void);
int snd_InsertSamplesFromLeadTime(int frameCount, int srcBufPtr, int samplesOfLeadTime);
int snd_PlaySamplesFromAtLength(int frameCount, int arrayIndex, int startIndex);
int snd_PlaySilence(void);
int snd_Start(int frameCount, int samplesPerSec, int stereo, int semaIndex);
int snd_Stop(void);

/* sound input */
int snd_SetRecordLevel(int level);
int snd_StartRecording(int desiredSamplesPerSec, int stereo, int semaIndex);
int snd_StopRecording(void);
double snd_GetRecordingSampleRate(void);
int snd_RecordSamplesIntoAtLength(int buf, int startSliceIndex, int bufferSizeInBytes);

/* joystick support */
int joystickInit(void);
int joystickRead(int stickIndex);

/* netscape plug-in support */
int plugInInit(char *imageName);
int plugInShutdown(void);
int plugInInterpretCycles(int cycleCount);

/* interpreter entry points needed by compiled primitives */
void * arrayValueOf(int arrayOop);
int checkedIntegerValueOf(int intOop);
void * fetchArrayofObject(int fieldIndex, int objectPointer);
double fetchFloatofObject(int fieldIndex, int objectPointer);
int fetchIntegerofObject(int fieldIndex, int objectPointer);
double floatValueOf(int floatOop);
void pop(int nItems);
void pushInteger(int integerValue);
int sizeOfSTArrayFromCPrimitive(void *cPtr);
void storeIntegerofObjectwithValue(int fieldIndex, int objectPointer, int integerValue);

/* sound generation primitives (old, for backward compatibility) */
int primWaveTableSoundmixSampleCountintostartingAtpan(void);
int primFMSoundmixSampleCountintostartingAtpan(void);
int primPluckedSoundmixSampleCountintostartingAtpan(void);
int primSampledSoundmixSampleCountintostartingAtpan(void);

/* sound generation primitives */
int primFMSoundmixSampleCountintostartingAtleftVolrightVol(void);
int primLoopedSampledSoundmixSampleCountintostartingAtleftVolrightVol(void);
int primPluckedSoundmixSampleCountintostartingAtleftVolrightVol(void);
int primReverbSoundapplyReverbTostartingAtcount(void);
int primSampledSoundmixSampleCountintostartingAtleftVolrightVol(void);

/* squeak socket record; see sqMacNetwork.c for details */
typedef struct {
	int		sessionID;
	int		socketType;  /* 0 = TCP, 1 = UDP */
	void	*privateSocketPtr;
}  SQSocket, *SocketPtr;

/* networking primitives */
int		sqNetworkInit(int resolverSemaIndex);
void	sqNetworkShutdown(void);
void	sqResolverAbort(void);
void	sqResolverAddrLookupResult(char *nameForAddress, int nameSize);
int		sqResolverAddrLookupResultSize(void);
int		sqResolverError(void);
int		sqResolverLocalAddress(void);
int		sqResolverNameLookupResult(void);
void	sqResolverStartAddrLookup(int address);
void	sqResolverStartNameLookup(char *hostName, int nameSize);
int		sqResolverStatus(void);
void	sqSocketAbortConnection(SocketPtr s);
void	sqSocketCloseConnection(SocketPtr s);
int		sqSocketConnectionStatus(SocketPtr s);
void	sqSocketConnectToPort(SocketPtr s, int addr, int port);
void	sqSocketCreateNetTypeSocketTypeRecvBytesSendBytesSemaID(
			SocketPtr s, int netType, int socketType,
			int recvBufSize, int sendBufSize, int semaIndex);
void	sqSocketDestroy(SocketPtr s);
int		sqSocketError(SocketPtr s);
void	sqSocketListenOnPort(SocketPtr s, int port);
int		sqSocketLocalAddress(SocketPtr s);
int		sqSocketLocalPort(SocketPtr s);
int		sqSocketReceiveDataAvailable(SocketPtr s);
int		sqSocketReceiveDataBufCount(SocketPtr s, int buf, int bufSize);
int		sqSocketRemoteAddress(SocketPtr s);
int		sqSocketRemotePort(SocketPtr s);
int		sqSocketSendDataBufCount(SocketPtr s, int buf, int bufSize);
int		sqSocketSendDone(SocketPtr s);

/* profiling */
int clearProfile(void);
int dumpProfile(void);
int startProfiling(void);
int stopProfiling(void);

/* system attributes */
int attributeSize(int id);
int getAttributeIntoLength(int id, int byteArrayIndex, int length);

/* miscellaneous primitives */
int primBitmapcompresstoByteArray(void);
int primBitmapdecompressfromByteArrayat(void);
int primSampledSoundconvert8bitSignedFromto16Bit(void);
int primStringcomparewithcollated(void);
int primStringfindFirstInStringinSetstartingAt(void);
int primStringfindSubstringinstartingAtmatchTable(void);
int primStringindexOfAsciiinStringstartingAt(void);
int primStringtranslatefromtotable(void);

/* serial port primitives */
int serialPortClose(int portNum);
int serialPortOpen(
  int portNum, int baudRate, int stopBitsType, int parityType, int dataBits,
  int inFlowCtrl, int outFlowCtrl, int xOnChar, int xOffChar);
int serialPortReadInto(int portNum, int count, int bufferPtr);
int serialPortWriteFrom(int portNum, int count, int bufferPtr);

/* MIDI primitives */
int sqMIDIGetClock(void);
int sqMIDIGetPortCount(void);
int sqMIDIGetPortDirectionality(int portNum);
int sqMIDIGetPortName(int portNum, int namePtr, int length);
int sqMIDIClosePort(int portNum);
int sqMIDIOpenPort(int portNum, int readSemaIndex, int interfaceClockRate);
int sqMIDIParameter(int whichParameter, int modify, int newValue);
int sqMIDIPortReadInto(int portNum, int count, int bufferPtr);
int sqMIDIPortWriteFromAt(int portNum, int count, int bufferPtr, int time);

/*** Experimental Asynchronous File I/O ***/
typedef struct {
	int			sessionID;
	void		*state;
} AsyncFile;

int asyncFileClose(AsyncFile *f);
int asyncFileOpen(AsyncFile *f, int fileNamePtr, int fileNameSize, int writeFlag, int semaIndex);
int asyncFileRecordSize();
int asyncFileReadResult(AsyncFile *f, int bufferPtr, int bufferSize);
int asyncFileReadStart(AsyncFile *f, int fPosition, int count);
int asyncFileWriteResult(AsyncFile *f);
int asyncFileWriteStart(AsyncFile *f, int fPosition, int bufferPtr, int bufferSize);

/*** pluggable primitive support ***/
int ioLoadExternalFunctionOfLengthFromModuleOfLength(
  int functionNameIndex, int functionNameLength,
  int moduleNameIndex, int moduleNameLength);

/* ROM Image file */
void sqImageFileClose(int f);
int sqImageFileOpen(char *fileName, char *mode);
int sqImageFilePosition(int f);
int sqImageFileRead(void *ptr, int elementSize, int count, int f);
void sqImageFileSeek(int f, int pos);

extern int specialObjectsOop;
extern int lastHash;
extern int savedWindowSize;
extern int fullScreenFlag;

#ifdef __cplusplus
};
#endif
