/* Automatically generated from Squeak on #(16 February 2019 11:13:17 pm) */

#include "msq.h"
#define error(x) ioError(x)

/* memory access macros */
#define byteAt(i) (*((unsigned char *) (i)))
#define byteAtput(i, val) (*((unsigned char *) (i)) = val)
#define longAt(i) (*((int *) (i)))
#define longAtput(i, val) (*((int *) (i)) = val)

/*** Variables ***/
int activeContext;
int affectedB;
int affectedL;
int affectedR;
int affectedT;
int allocationCount;
int allocationsBetweenGCs;
int argumentCount;
int atCache[65];
int bbH;
int bbW;
int bitBltOop;
int bitCount;
int child;
int clipHeight;
int clipWidth;
int clipX;
int clipY;
int cmBitsPerColor;
int colorMap;
int combinationRule;
int compEnd;
int compStart;
int deferDisplayUpdates;
int destBits;
int destDelta;
int destForm;
int destIndex;
int destMask;
int destPixSize;
int destRaster;
int destX;
int destY;
int displayBits;
int dx;
int dy;
int endOfMemory;
int falseObj;
int field;
int freeBlock;
int freeContexts;
int fullScreenFlag;
int fwdTableLast;
int fwdTableNext;
int hDir;
int halftoneBase;
int halftoneForm;
int halftoneHeight;
int height;
int instructionPointer;
int interpreterProxy;
int interruptCheckCounter;
int interruptKeycode;
int interruptPending;
int lastHash;
int lastTick;
int lkupClass;
int lowSpaceThreshold;
int mask1;
int mask2;
int maskTable[33] = {
0, 1, 3, 0, 15, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0, 65535,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1
};
unsigned char *memory;
int memoryLimit;
int messageSelector;
int method;
int methodCache[1025];
int nWords;
int newMethod;
int nextPollTick;
int nextWakeupTick;
int nilObj;
int noHalftone;
int noSource;
int opTable[36];
int parentField;
int pendingFinalizationSignals;
int pixPerWord;
int preload;
int primitiveIndex;
int receiver;
int reclaimableContextCount;
int remapBuffer[6];
int remapBufferCount;
int rootTable[501];
int rootTableCount;
int savedWindowSize;
int scanDisplayFlag;
int scanRightX;
int scanStart;
int scanStop;
int scanStopArray;
int scanString;
int scanXTable;
int semaphoresToSignal[6];
int semaphoresToSignalCount;
int signalLowSpace;
int skew;
int sourceAlpha;
int sourceBits;
int sourceDelta;
int sourceForm;
int sourceIndex;
int sourcePixSize;
int sourceRaster;
int sourceX;
int sourceY;
int specialObjectsOop;
int srcBitIndex;
int srcHeight;
int srcWidth;
int stackPointer;
int statFullGCMSecs;
int statFullGCs;
int statIncrGCMSecs;
int statIncrGCs;
int statRootTableOverflows;
int statTenures;
int stopCode;
int successFlag;
int sx;
int sy;
int tenuringThreshold;
int theHomeContext;
int trueObj;
int vDir;
int width;
int youngStart;

/*** Function Prototypes ***/
int OLDrgbDiffwith(int sourceWord, int destinationWord);
int OLDtallyIntoMapwith(int sourceWord, int destinationWord);
void aComment(void);
void aFinalizationComment(void);
int accessibleObjectAfter(int oop);
void activateNewMethod(void);
void addLastLinktoList(int proc, int aList);
int addToMethodCacheSelclassmethodprimIndex(int selector, int cls, int meth, int primIndex);
int addWordwith(int sourceWord, int destinationWord);
void adjustAllOopsBy(int bytesToShift);
void adjustFieldsAndClassOfby(int oop, int offsetBytes);
int affectedBottom(void);
int affectedLeft(void);
int affectedRight(void);
int affectedTop(void);
void allAccessibleObjectsOkay(void);
int allYoungand(int array1, int array2);
int allocateheaderSizeh1h2h3doFillwith(int byteSize, int hdrSize, int baseHeader, int classOop, int extendedSize, int doFill, int fillWord);
int allocateChunk(int byteSize);
int allocateOrRecycleContext(void);
int alphaBlendwith(int sourceWord, int destinationWord);
int alphaBlendConstwith(int sourceWord, int destinationWord);
int alphaBlendConstwithpaintMode(int sourceWord, int destinationWord, int paintMode);
int alphaBlendScaledwith(int sourceWord, int destinationWord);
int alphaPaintConstwith(int sourceWord, int destinationWord);
void alphaSourceBlendBits16(void);
void alphaSourceBlendBits32(void);
void alphaSourceBlendBits8(void);
int areIntegersand(int oop1, int oop2);
int argCount(void);
int argumentCountOf(int methodPointer);
int argumentCountOfBlock(int blockPointer);
void * arrayValueOf(int arrayOop);
int asciiDirectoryDelimiter(void);
int asciiOfCharacter(int characterObj);
int baseHeader(int oop);
void beRootIfOld(int oop);
void beRootWhileForwarding(int oop);
int becomewithtwoWay(int array1, int array2, int twoWayFlag);
int bitAndwith(int sourceWord, int destinationWord);
int bitAndInvertwith(int sourceWord, int destinationWord);
int bitInvertAndwith(int sourceWord, int destinationWord);
int bitInvertAndInvertwith(int sourceWord, int destinationWord);
int bitInvertDestinationwith(int sourceWord, int destinationWord);
int bitInvertOrwith(int sourceWord, int destinationWord);
int bitInvertOrInvertwith(int sourceWord, int destinationWord);
int bitInvertSourcewith(int sourceWord, int destinationWord);
int bitInvertXorwith(int sourceWord, int destinationWord);
int bitOrwith(int sourceWord, int destinationWord);
int bitOrInvertwith(int sourceWord, int destinationWord);
int bitXorwith(int sourceWord, int destinationWord);
int booleanValueOf(int obj);
int byteLengthOf(int oop);
void byteSwapByteObjects(void);
int byteSwapped(int w);
int caller(void);
int characterForAscii(int ascii);
void checkAddress(int byteAddress);
void checkBooleanResult(int result);
void checkForInterrupts(void);
int checkImageVersionFrom(int f);
void checkSourceOverlap(void);
int checkedByteAt(int byteAddress);
void checkedByteAtput(int byteAddress, int byte);
int checkedIntegerValueOf(int intOop);
int checkedLongAt(int byteAddress);
void checkedLongAtput(int byteAddress, int a32BitInteger);
int chunkFromOop(int oop);
int classHeader(int oop);
void cleanUpContexts(void);
void clearRootsTable(void);
int clearWordwith(int source, int destination);
int clipRange(void);
int clone(int oop);
int commonAt(int stringy);
int commonAtPut(int stringy);
int commonVariableatcacheIndex(int rcvr, int index, int atIx);
int commonVariableatputcacheIndex(int rcvr, int index, int value, int atIx);
int compare31or32Bitsequal(int obj1, int obj2);
int containOnlyOopsand(int array1, int array2);
int copyBits(void);
void copyBitsFromtoat(int startX, int stopX, int yValue);
void copyLoop(void);
void copyLoopNoSource(void);
void copyLoopPixMap(void);
void createActualMessage(void);
unsigned int * default8To32Table(void);
int deltaFromtonSteps(int x1, int x2, int n);
void destMaskAndPointerInit(void);
int destinationWordwith(int sourceWord, int destinationWord);
int doPrimitiveDivby(int rcvr, int arg);
int doPrimitiveModby(int rcvr, int arg);
void drawLoopXY(int xDelta, int yDelta);
void exchangeHashBitswith(int oop1, int oop2);
int executeNewMethod(void);
int extraHeaderBytes(int oopOrChunk);
int failed(void);
int fetchByteofObject(int byteIndex, int oop);
int fetchClassOf(int oop);
int fetchClassOfNonInt(int oop);
void fetchContextRegisters(int activeCntx);
int fetchIntegerofObject(int fieldIndex, int objectPointer);
int fetchIntegerOrTruncFloatofObject(int fieldIndex, int objectPointer);
int fetchPointerofObject(int fieldIndex, int oop);
int fetchStackPointerOf(int aContext);
int fetchWordofObject(int fieldIndex, int oop);
int fetchWordLengthOf(int objectPointer);
int fileRecordSize(void);
SQFile * fileValueOf(int objectPointer);
void finalizeReference(int oop);
int findClassOfMethodforReceiver(int meth, int rcvr);
void findNewMethodInClass(int cls);
int findSelectorOfMethodforReceiver(int meth, int rcvr);
int firstAccessibleObject(void);
int firstObject(void);
int fixedFieldsOfformatlength(int oop, int fmt, int wordLength);
int floatObjectOf(double aFloat);
double floatValueOf(int oop);
void flushMethodCache(void);
int formatOf(int oop);
int formatOfClass(int classPointer);
void fullCompaction(void);
void fullDisplayUpdate(void);
void fullGC(void);
int fwdBlockGet(int blkSize);
void fwdBlockValidate(int addr);
int fwdTableInit(int blkSize);
int getLongFromFileswap(int f, int swapFlag);
int hashBitsOf(int oop);
int headerOf(int methodPointer);
int headerType(int oop);
int ignoreSourceOrHalftone(int formPointer);
int imageFormatVersion(void);
int incCompBody(void);
int incCompMakeFwd(void);
int incCompMove(int bytesFreed);
void incrementalCompaction(void);
void incrementalGC(void);
void initBBOpTable(void);
void initForwardBlockmappingtowithBackPtr(int fwdBlock, int oop, int newOop, int backFlag);
int initialInstanceOf(int classPointer);
void initializeInterpreter(int bytesToShift);
void initializeMemoryFirstFree(int firstFree);
void initializeObjectMemory(int bytesToShift);
int installinAtCacheatstring(int rcvr, int *cache, int atIx, int stringy);
int instanceAfter(int objectPointer);
int instantiateClassindexableSize(int classPointer, int size);
int instantiateContextsizeInBytes(int classPointer, int sizeInBytes);
int instantiateSmallClasssizeInBytesfill(int classPointer, int sizeInBytes, int fillValue);
int integerObjectOf(int value);
int integerValueOf(int objectPointer);
void interpret(void);
int isBytes(int oop);
int isContextHeader(int aHeader);
int isEmptyList(int aLinkedList);
int isFreeObject(int oop);
int isIntegerObject(int objectPointer);
int isIntegerValue(int intValue);
int isMethodContextHeader(int aHeader);
int isObjectForwarded(int oop);
int isPointers(int oop);
int isWeak(int oop);
int isWords(int oop);
int isWordsOrBytes(int oop);
int lastPointerOf(int oop);
int lastPointerWhileForwarding(int oop);
int lengthOf(int oop);
int lengthOfbaseHeaderformat(int oop, int hdr, int fmt);
int literal(int offset);
int literalofMethod(int offset, int methodPointer);
int literalCountOf(int methodPointer);
int literalCountOfHeader(int headerPointer);
int loadBitBltFrom(int bbObj);
double loadFloatOrIntFrom(int floatOrInt);
void loadInitialContext(void);
int loadScannerFromstartstopstringrightXstopArraydisplayFlag(int bbObj, int start, int stop, int string, int rightX, int stopArray, int displayFlag);
int lookupInMethodCacheSelclass(int selector, int cls);
int lookupMethodInClass(int cls);
int lookupMethodInDictionary(int dictionary);
int lowestFreeAfter(int chunk);
int makePointwithxValueyValue(int xValue, int yValue);
void mapInterpreterOops(void);
void mapPointersInObjectsFromto(int memStart, int memEnd);
void markAndTrace(int oop);
void markAndTraceInterpreterOops(void);
void markPhase(void);
int mergewith(int sourceWord, int destinationWord);
int methodClassOf(int methodPointer);
void newActiveContext(int aContext);
int newObjectHash(void);
int nilObject(void);
int nonWeakFieldsOf(int oop);
int objectAfter(int oop);
int objectAfterWhileForwarding(int oop);
void okayActiveProcessStack(void);
int okayFields(int oop);
void okayInterpreterObjects(void);
int okayOop(int oop);
int oopFromChunk(int chunk);
int oopHasOkayClass(int oop);
int partitionedANDtonBitsnPartitions(int word1, int word2, int nBits, int nParts);
int partitionedAddtonBitsnPartitions(int word1, int word2, int nBits, int nParts);
int partitionedMaxwithnBitsnPartitions(int word1, int word2, int nBits, int nParts);
int partitionedMinwithnBitsnPartitions(int word1, int word2, int nBits, int nParts);
int partitionedSubfromnBitsnPartitions(int word1, int word2, int nBits, int nParts);
int pickSourcePixelsnullMapsrcMaskdestMask(int nPix, int nullMap, int sourcePixMask, int destPixMask);
int pickSourcePixelssrcMaskdestMask(int nPix, int sourcePixMask, int destPixMask);
int pickSourcePixelsNullMapsrcMaskdestMask(int nPix, int sourcePixMask, int destPixMask);
int pickSourcePixelsRGBnullMapsrcMaskdestMask(int nPix, int nullMap, int sourcePixMask, int destPixMask);
int pixMaskwith(int sourceWord, int destinationWord);
int pixPaintwith(int sourceWord, int destinationWord);
void pop2AndPushIntegerIfOK(int integerResult);
void pop(int nItems);
int popthenPush(int nItems, int oop);
double popFloat(void);
int popInteger(void);
int popPos32BitInteger(void);
int popRemappableOop(void);
int popStack(void);
int positive32BitIntegerFor(int integerValue);
int positive32BitValueOf(int oop);
void possibleRootStoreIntovalue(int oop, int valueObj);
void postGCAction(void);
void preGCAction(void);
int prepareForwardingTableForBecomingwithtwoWay(int array1, int array2, int twoWayFlag);
int primIndex(void);
void primitiveAdd(void);
void primitiveArctan(void);
void primitiveArrayBecome(void);
void primitiveArrayBecomeOneWay(void);
void primitiveAsFloat(void);
void primitiveAsOop(void);
void primitiveAt(void);
void primitiveAtEnd(void);
void primitiveAtPut(void);
void primitiveBeCursor(void);
void primitiveBeDisplay(void);
void primitiveBeep(void);
void primitiveBitAnd(void);
void primitiveBitOr(void);
void primitiveBitShift(void);
void primitiveBitXor(void);
void primitiveBlockCopy(void);
void primitiveBytesLeft(void);
void primitiveClass(void);
void primitiveClone(void);
void primitiveConstantFill(void);
void primitiveCopyBits(void);
void primitiveDeferDisplayUpdates(void);
void primitiveDirectoryDelimitor(void);
void primitiveDiv(void);
void primitiveDivide(void);
int primitiveDoPrimitiveWithArgs(void);
void primitiveDrawLoop(void);
void primitiveEqual(void);
void primitiveEquivalent(void);
void primitiveExitToDebugger(void);
void primitiveExp(void);
void primitiveExponent(void);
int primitiveFail(void);
void primitiveFileAtEnd(void);
void primitiveFileClose(void);
void primitiveFileDelete(void);
void primitiveFileGetPosition(void);
void primitiveFileOpen(void);
void primitiveFileRead(void);
void primitiveFileRename(void);
void primitiveFileSetPosition(void);
void primitiveFileSize(void);
void primitiveFileWrite(void);
void primitiveFloatAdd(void);
void primitiveFloatAddtoArg(int rcvrOop, int argOop);
void primitiveFloatDivide(void);
void primitiveFloatDividebyArg(int rcvrOop, int argOop);
void primitiveFloatEqual(void);
int primitiveFloatEqualtoArg(int rcvrOop, int argOop);
int primitiveFloatGreaterthanArg(int rcvrOop, int argOop);
void primitiveFloatGreaterOrEqual(void);
void primitiveFloatGreaterThan(void);
int primitiveFloatLessthanArg(int rcvrOop, int argOop);
void primitiveFloatLessOrEqual(void);
void primitiveFloatLessThan(void);
void primitiveFloatMultiply(void);
void primitiveFloatMultiplybyArg(int rcvrOop, int argOop);
void primitiveFloatNotEqual(void);
void primitiveFloatSubtract(void);
void primitiveFloatSubtractfromArg(int rcvrOop, int argOop);
void primitiveFlushCache(void);
void primitiveFlushCacheByMethod(void);
void primitiveFlushCacheSelective(void);
void primitiveFractionalPart(void);
void primitiveFullGC(void);
void primitiveGetAttribute(void);
void primitiveGreaterOrEqual(void);
void primitiveGreaterThan(void);
void primitiveImageName(void);
void primitiveIncrementalGC(void);
int primitiveIndexOf(int methodPointer);
void primitiveInstVarAt(void);
void primitiveInstVarAtPut(void);
void primitiveInterruptSemaphore(void);
void primitiveKbdNext(void);
void primitiveKbdPeek(void);
void primitiveLessOrEqual(void);
void primitiveLessThan(void);
void primitiveLoadInstVar(void);
void primitiveLogN(void);
void primitiveLowSpaceSemaphore(void);
void primitiveMakePoint(void);
void primitiveMillisecondClock(void);
void primitiveMod(void);
void primitiveMouseButtons(void);
void primitiveMousePoint(void);
void primitiveMultiply(void);
void primitiveNew(void);
void primitiveNewMethod(void);
void primitiveNewWithArg(void);
int primitiveNext(void);
void primitiveNextInstance(void);
void primitiveNextObject(void);
int primitiveNextPut(void);
void primitiveNotEqual(void);
void primitiveObjectAt(void);
void primitiveObjectAtPut(void);
int primitiveObjectPointsTo(void);
void primitivePerform(void);
void primitivePerformWithArgs(void);
void primitivePointX(void);
void primitivePointY(void);
void primitivePushFalse(void);
void primitivePushMinusOne(void);
void primitivePushNil(void);
void primitivePushOne(void);
void primitivePushSelf(void);
void primitivePushTrue(void);
void primitivePushTwo(void);
void primitivePushZero(void);
int primitivePutChar(void);
void primitiveQuit(void);
void primitiveQuo(void);
int primitiveResponse(void);
void primitiveResume(void);
void primitiveScreenSize(void);
void primitiveSecondsClock(void);
void primitiveSetFullScreen(void);
void primitiveSetInterruptKey(void);
int primitiveShortAt(void);
int primitiveShortAtPut(void);
void primitiveShowDisplayRect(void);
void primitiveSignal(void);
void primitiveSignalAtBytesLeft(void);
void primitiveSignalAtMilliseconds(void);
void primitiveSine(void);
void primitiveSize(void);
void primitiveSomeInstance(void);
void primitiveSomeObject(void);
void primitiveSoundStop(void);
void primitiveSpecialObjectsOop(void);
void primitiveSquareRoot(void);
int primitiveStoreStackp(void);
void primitiveStringAt(void);
void primitiveStringAtPut(void);
int primitiveStringReplace(void);
void primitiveSubtract(void);
void primitiveSuspend(void);
void primitiveTimesTwoPower(void);
void primitiveTruncated(void);
int primitiveVMParameter(void);
void primitiveValue(void);
void primitiveValueWithArgs(void);
void primitiveWait(void);
void primitiveWarpBits(void);
void print(const char *s);
void printCallStack(void);
void printCallStackFrom(int aContext);
void printNameOfClasscount(int classOop, int cnt);
int printStringOf(int oop);
void push(int object);
int pushBool(int trueOrFalse);
void pushFloat(double f);
void pushInteger(int integerValue);
void pushRemappableOop(int oop);
void putToSleep(int aProcess);
void quickCheckForInterrupts(void);
int quickFetchIntegerofObject(int fieldIndex, int objectPointer);
int readImageFromFileHeapSize(int f, int desiredHeapSize);
int readableFormat(int imageVersion);
void recycleContextIfPossible(int cntxOop);
int remap(int oop);
int remapClassOf(int oop);
void remapFieldsAndClassOf(int oop);
int removeFirstLinkOfList(int aList);
void restoreHeaderOf(int oop);
void restoreHeadersAfterBecomingwith(int list1, int list2);
void restoreHeadersAfterForwardBecome(void);
void resume(int aProcess);
int returnAtlastIndexlefttop(int stopIndex, int lastIndex, int left, int top);
void reverseBytesFromto(int startAddr, int stopAddr);
void reverseBytesInImage(void);
int rgbAddwith(int sourceWord, int destinationWord);
int rgbDiffwith(int sourceWord, int destinationWord);
int rgbMapfromto(int sourcePixel, int nBitsIn, int nBitsOut);
int rgbMaxwith(int sourceWord, int destinationWord);
int rgbMinwith(int sourceWord, int destinationWord);
int rgbMinInvertwith(int wordToInvert, int destinationWord);
int rgbSubwith(int sourceWord, int destinationWord);
int rightType(int headerWord);
int scanCharacters(void);
int schedulerPointer(void);
void setSizeOfFreeto(int chunk, int byteSize);
int showDisplayBits(void);
int signExtend16(int int16);
void signalFinalization(int weakReferenceOop);
int signalSemaphoreWithIndex(int index);
int sizeBitsOf(int oop);
int sizeBitsOfSafe(int oop);
int sizeHeader(int oop);
int sizeOfFree(int oop);
int sizeOfSTArrayFromCPrimitive(void *cPtr);
int smoothPixatXfyfdxhdyhdxvdyvpixPerWordpixelMasksourceMap(int n, int xf, int yf, int dxh, int dyh, int dxv, int dyv, int srcPixPerWord, int sourcePixMask, int sourceMap);
int sourcePixAtXypixPerWord(int x, int y, int srcPixPerWord);
void sourceSkewAndPointerInit(void);
int sourceWordwith(int sourceWord, int destinationWord);
int specialSelector(int index);
int splObj(int index);
int stObjectat(int array, int index);
void stObjectatput(int array, int index, int value);
int stSizeOf(int oop);
int stackIntegerValue(int offset);
int stackPointerIndex(void);
int stackTop(void);
int stackValue(int offset);
int startField(void);
int startObj(void);
int startOfMemory(void);
int stopReason(void);
int storeByteofObjectwithValue(int byteIndex, int oop, int valueByte);
void storeContextRegisters(int activeCntx);
void storeIntegerofObjectwithValue(int fieldIndex, int objectPointer, int integerValue);
int storePointerofObjectwithValue(int fieldIndex, int oop, int valuePointer);
int storePointerUncheckedofObjectwithValue(int fieldIndex, int oop, int valuePointer);
void storeStackPointerValueinContext(int value, int contextPointer);
int storeWordofObjectwithValue(int fieldIndex, int oop, int valueWord);
int subWordwith(int sourceWord, int destinationWord);
int subscriptwithformat(int array, int index, int fmt);
void subscriptwithstoringformat(int array, int index, int oopToStore, int fmt);
int success(int successValue);
int sufficientSpaceAfterGC(int minFree);
int sufficientSpaceToAllocate(int bytes);
int sufficientSpaceToInstantiateindexableSize(int classOop, int size);
int superclassOf(int classPointer);
int sweepPhase(void);
void synchronousSignal(int aSemaphore);
int tallyIntoMapwith(int sourceWord, int destinationWord);
int targetForm(void);
void transferfromto(int count, int src, int dst);
void transferfromIndexofObjecttoIndexofObject(int count, int firstFrom, int fromOop, int firstTo, int toOop);
void transferTo(int aProc);
int tryCopyingBitsQuickly(void);
void unPop(int nItems);
void unknownBytecode(void);
int upward(void);
int wakeHighestPriority(void);
int warpBits(void);
int warpLoop(void);
int warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(int nPix, int xDeltah, int yDeltah, int xDeltav, int yDeltav, int n, int sourceMapOop);

#ifdef __cplus
#pragma GCC diagnostic ignored "-fpermissive"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-value"
#pragma GCC diagnostic ignored "-Wreturn-type"

int OLDrgbDiffwith(int sourceWord, int destinationWord) {
    int diff;
    int pixMask;

	if (destPixSize < 16) {
		diff = sourceWord ^ destinationWord;
		pixMask = maskTable[destPixSize];
		while (!(diff == 0)) {
			if ((diff & pixMask) != 0) {
				bitCount += 1;
			}
			diff = ((unsigned) diff) >> destPixSize;
		}
		return destinationWord;
	}
	if (destPixSize == 16) {
		diff = partitionedSubfromnBitsnPartitions(sourceWord, destinationWord, 5, 3);
		bitCount = ((bitCount + (diff & 31)) + ((((unsigned) diff) >> 5) & 31)) + ((((unsigned) diff) >> 10) & 31);
		diff = partitionedSubfromnBitsnPartitions(((unsigned) sourceWord) >> 16, ((unsigned) destinationWord) >> 16, 5, 3);
		bitCount = ((bitCount + (diff & 31)) + ((((unsigned) diff) >> 5) & 31)) + ((((unsigned) diff) >> 10) & 31);
	} else {
		diff = partitionedSubfromnBitsnPartitions(sourceWord, destinationWord, 8, 3);
		bitCount = ((bitCount + (diff & 255)) + ((((unsigned) diff) >> 8) & 255)) + ((((unsigned) diff) >> 16) & 255);
	}
	return destinationWord;
}

int OLDtallyIntoMapwith(int sourceWord, int destinationWord) {
    int mapIndex;
    int pixMask;
    int shiftWord;
    int i;
    int mask;
    int srcPix;
    int destPix;
    int d;
    int mask3;
    int srcPix1;
    int destPix1;
    int d1;
    int mask4;
    int srcPix2;
    int destPix2;
    int d2;

	if (colorMap == nilObj) {
		return destinationWord;
	}
	if (destPixSize < 16) {
		pixMask = maskTable[destPixSize];
		shiftWord = destinationWord;
		for (i = 1; i <= pixPerWord; i += 1) {
			mapIndex = shiftWord & pixMask;
			longAtput(((((char *) colorMap)) + 4) + (mapIndex << 2), (longAt(((((char *) colorMap)) + 4) + (mapIndex << 2))) + 1);
			shiftWord = ((unsigned) shiftWord) >> destPixSize;
		}
		return destinationWord;
	}
	if (destPixSize == 16) {
		/* begin rgbMap:from:to: */
		if ((d = cmBitsPerColor - 5) > 0) {
			mask = (1 << 5) - 1;
			srcPix = (destinationWord & 65535) << d;
			mask = mask << d;
			destPix = srcPix & mask;
			mask = mask << cmBitsPerColor;
			srcPix = srcPix << d;
			mapIndex = (destPix + (srcPix & mask)) + ((srcPix << d) & (mask << cmBitsPerColor));
			goto l1;
		} else {
			if (d == 0) {
				mapIndex = destinationWord & 65535;
				goto l1;
			}
			if ((destinationWord & 65535) == 0) {
				mapIndex = destinationWord & 65535;
				goto l1;
			}
			d = 5 - cmBitsPerColor;
			mask = (1 << cmBitsPerColor) - 1;
			srcPix = ((unsigned) (destinationWord & 65535)) >> d;
			destPix = srcPix & mask;
			mask = mask << cmBitsPerColor;
			srcPix = ((unsigned) srcPix) >> d;
			destPix = (destPix + (srcPix & mask)) + ((((unsigned) srcPix) >> d) & (mask << cmBitsPerColor));
			if (destPix == 0) {
				mapIndex = 1;
				goto l1;
			}
			mapIndex = destPix;
			goto l1;
		}
	l1:	/* end rgbMap:from:to: */;
		longAtput(((((char *) colorMap)) + 4) + (mapIndex << 2), (longAt(((((char *) colorMap)) + 4) + (mapIndex << 2))) + 1);
		/* begin rgbMap:from:to: */
		if ((d1 = cmBitsPerColor - 5) > 0) {
			mask3 = (1 << 5) - 1;
			srcPix1 = (((unsigned) destinationWord) >> 16) << d1;
			mask3 = mask3 << d1;
			destPix1 = srcPix1 & mask3;
			mask3 = mask3 << cmBitsPerColor;
			srcPix1 = srcPix1 << d1;
			mapIndex = (destPix1 + (srcPix1 & mask3)) + ((srcPix1 << d1) & (mask3 << cmBitsPerColor));
			goto l2;
		} else {
			if (d1 == 0) {
				mapIndex = ((unsigned) destinationWord) >> 16;
				goto l2;
			}
			if ((((unsigned) destinationWord) >> 16) == 0) {
				mapIndex = ((unsigned) destinationWord) >> 16;
				goto l2;
			}
			d1 = 5 - cmBitsPerColor;
			mask3 = (1 << cmBitsPerColor) - 1;
			srcPix1 = ((unsigned) (((unsigned) destinationWord) >> 16)) >> d1;
			destPix1 = srcPix1 & mask3;
			mask3 = mask3 << cmBitsPerColor;
			srcPix1 = ((unsigned) srcPix1) >> d1;
			destPix1 = (destPix1 + (srcPix1 & mask3)) + ((((unsigned) srcPix1) >> d1) & (mask3 << cmBitsPerColor));
			if (destPix1 == 0) {
				mapIndex = 1;
				goto l2;
			}
			mapIndex = destPix1;
			goto l2;
		}
	l2:	/* end rgbMap:from:to: */;
		longAtput(((((char *) colorMap)) + 4) + (mapIndex << 2), (longAt(((((char *) colorMap)) + 4) + (mapIndex << 2))) + 1);
	} else {
		/* begin rgbMap:from:to: */
		if ((d2 = cmBitsPerColor - 8) > 0) {
			mask4 = (1 << 8) - 1;
			srcPix2 = destinationWord << d2;
			mask4 = mask4 << d2;
			destPix2 = srcPix2 & mask4;
			mask4 = mask4 << cmBitsPerColor;
			srcPix2 = srcPix2 << d2;
			mapIndex = (destPix2 + (srcPix2 & mask4)) + ((srcPix2 << d2) & (mask4 << cmBitsPerColor));
			goto l3;
		} else {
			if (d2 == 0) {
				mapIndex = destinationWord;
				goto l3;
			}
			if (destinationWord == 0) {
				mapIndex = destinationWord;
				goto l3;
			}
			d2 = 8 - cmBitsPerColor;
			mask4 = (1 << cmBitsPerColor) - 1;
			srcPix2 = ((unsigned) destinationWord) >> d2;
			destPix2 = srcPix2 & mask4;
			mask4 = mask4 << cmBitsPerColor;
			srcPix2 = ((unsigned) srcPix2) >> d2;
			destPix2 = (destPix2 + (srcPix2 & mask4)) + ((((unsigned) srcPix2) >> d2) & (mask4 << cmBitsPerColor));
			if (destPix2 == 0) {
				mapIndex = 1;
				goto l3;
			}
			mapIndex = destPix2;
			goto l3;
		}
	l3:	/* end rgbMap:from:to: */;
		longAtput(((((char *) colorMap)) + 4) + (mapIndex << 2), (longAt(((((char *) colorMap)) + 4) + (mapIndex << 2))) + 1);
	}
	return destinationWord;
}

void aComment(void) {
}

void aFinalizationComment(void) {
	error("Comment only");
}

int accessibleObjectAfter(int oop) {
    int obj;
    int sz;
    int header;
    int extra;
    int type;
    int extra1;
    int sz1;
    int header1;
    int extra2;
    int type1;
    int extra11;

	/* begin objectAfter: */
	;
	if (((longAt(oop)) & 3) == 2) {
		sz1 = (longAt(oop)) & 4294967292U;
	} else {
		/* begin sizeBitsOf: */
		header1 = longAt(oop);
		if ((header1 & 3) == 0) {
			sz1 = (longAt(oop - 8)) & 4294967292U;
			goto l2;
		} else {
			sz1 = header1 & 252;
			goto l2;
		}
	l2:	/* end sizeBitsOf: */;
	}
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type1 = (longAt(oop + sz1)) & 3;
	if (type1 > 1) {
		extra11 = 0;
	} else {
		if (type1 == 1) {
			extra11 = 4;
		} else {
			extra11 = 8;
		}
	}
	extra2 = extra11;
	obj = (oop + sz1) + extra2;
	while (obj < endOfMemory) {
		if (!(((longAt(obj)) & 3) == 2)) {
			return obj;
		}
		/* begin objectAfter: */
		;
		if (((longAt(obj)) & 3) == 2) {
			sz = (longAt(obj)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header = longAt(obj);
			if ((header & 3) == 0) {
				sz = (longAt(obj - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type = (longAt(obj + sz)) & 3;
		if (type > 1) {
			extra1 = 0;
		} else {
			if (type == 1) {
				extra1 = 4;
			} else {
				extra1 = 8;
			}
		}
		extra = extra1;
		obj = (obj + sz) + extra;
	}
	return null;
}

void activateNewMethod(void) {
    int methodHeader;
    int i;
    int nilOop;
    int tempCount;
    int newContext;
    int initialIP;
    int cntxt;
    int tmp;

	methodHeader = longAt(((((char *) newMethod)) + 4) + (0 << 2));
	/* begin allocateOrRecycleContext */
	if (freeContexts != 1) {
		cntxt = freeContexts;
		freeContexts = longAt(((((char *) cntxt)) + 4) + (0 << 2));
		newContext = cntxt;
		goto l1;
	}
	cntxt = instantiateContextsizeInBytes(longAt(((((char *) specialObjectsOop)) + 4) + (10 << 2)), 156);
	longAtput(((((char *) cntxt)) + 4) + (4 << 2), nilObj);
	newContext = cntxt;
l1:	/* end allocateOrRecycleContext */;
	initialIP = ((1 + ((((unsigned) methodHeader) >> 10) & 255)) * 4) + 1;
	tempCount = (((unsigned) methodHeader) >> 19) & 63;
	longAtput(((((char *) newContext)) + 4) + (0 << 2), activeContext);
	longAtput(((((char *) newContext)) + 4) + (1 << 2), ((initialIP << 1) | 1));
	longAtput(((((char *) newContext)) + 4) + (2 << 2), ((tempCount << 1) | 1));
	longAtput(((((char *) newContext)) + 4) + (3 << 2), newMethod);
	for (i = 0; i <= argumentCount; i += 1) {
		longAtput(((((char *) newContext)) + 4) + ((5 + i) << 2), longAt(stackPointer - ((argumentCount - i) * 4)));
	}
	nilOop = nilObj;
	for (i = (argumentCount + 1); i <= tempCount; i += 1) {
		longAtput(((((char *) newContext)) + 4) + ((5 + i) << 2), nilOop);
	}
	/* begin pop: */
	stackPointer -= (argumentCount + 1) * 4;
	reclaimableContextCount += 1;
	/* begin newActiveContext: */
	/* begin storeContextRegisters: */
	longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
	longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
	if (newContext < youngStart) {
		beRootIfOld(newContext);
	}
	activeContext = newContext;
	/* begin fetchContextRegisters: */
	tmp = longAt(((((char *) newContext)) + 4) + (3 << 2));
	if ((tmp & 1)) {
		tmp = longAt(((((char *) newContext)) + 4) + (5 << 2));
		if (tmp < youngStart) {
			beRootIfOld(tmp);
		}
	} else {
		tmp = newContext;
	}
	theHomeContext = tmp;
	receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
	method = longAt(((((char *) tmp)) + 4) + (3 << 2));
	tmp = ((longAt(((((char *) newContext)) + 4) + (1 << 2))) >> 1);
	instructionPointer = ((method + tmp) + 4) - 2;
	tmp = ((longAt(((((char *) newContext)) + 4) + (2 << 2))) >> 1);
	stackPointer = (newContext + 4) + (((6 + tmp) - 1) * 4);
}

void addLastLinktoList(int proc, int aList) {
    int lastLink;

	if ((longAt(((((char *) aList)) + 4) + (0 << 2))) == nilObj) {
		/* begin storePointer:ofObject:withValue: */
		if (aList < youngStart) {
			possibleRootStoreIntovalue(aList, proc);
		}
		longAtput(((((char *) aList)) + 4) + (0 << 2), proc);
	} else {
		lastLink = longAt(((((char *) aList)) + 4) + (1 << 2));
		/* begin storePointer:ofObject:withValue: */
		if (lastLink < youngStart) {
			possibleRootStoreIntovalue(lastLink, proc);
		}
		longAtput(((((char *) lastLink)) + 4) + (0 << 2), proc);
	}
	/* begin storePointer:ofObject:withValue: */
	if (aList < youngStart) {
		possibleRootStoreIntovalue(aList, proc);
	}
	longAtput(((((char *) aList)) + 4) + (1 << 2), proc);
	/* begin storePointer:ofObject:withValue: */
	if (proc < youngStart) {
		possibleRootStoreIntovalue(proc, aList);
	}
	longAtput(((((char *) proc)) + 4) + (3 << 2), aList);
}

int addToMethodCacheSelclassmethodprimIndex(int selector, int cls, int meth, int primIndex) {
    int probe;
    int p;
    int hash;

	hash = selector ^ cls;
	for (p = 0; p <= (3 - 1); p += 1) {
		probe = (((unsigned) hash) >> p) & 1020;
		if ((methodCache[probe + 1]) == 0) {
			methodCache[probe + 1] = selector;
			methodCache[probe + 2] = cls;
			methodCache[probe + 3] = meth;
			methodCache[probe + 4] = primIndex;
			return null;
		}
	}
	probe = hash & 1020;
	methodCache[probe + 1] = selector;
	methodCache[probe + 2] = cls;
	methodCache[probe + 3] = meth;
	methodCache[probe + 4] = primIndex;
	for (p = 1; p <= (3 - 1); p += 1) {
		probe = (((unsigned) hash) >> p) & 1020;
		methodCache[probe + 1] = 0;
	}
}

int addWordwith(int sourceWord, int destinationWord) {
	return sourceWord + destinationWord;
}

void adjustAllOopsBy(int bytesToShift) {
    int oop;
    int header;
    int newClassOop;
    int fieldAddr;
    int fieldOop;
    int classHeader;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header1;
    int extra2;
    int type1;
    int extra11;

	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	oop = chunk + extra;
	while (oop < endOfMemory) {
		if (!(((longAt(oop)) & 3) == 2)) {
			header = longAt(oop);
			longAtput(oop, header & 3221225471U);
			/* begin adjustFieldsAndClassOf:by: */
			fieldAddr = oop + (lastPointerOf(oop));
			while (fieldAddr > oop) {
				fieldOop = longAt(fieldAddr);
				if (!((fieldOop & 1))) {
					longAtput(fieldAddr, fieldOop + bytesToShift);
				}
				fieldAddr -= 4;
			}
			if (((longAt(oop)) & 3) != 3) {
				classHeader = longAt(oop - 4);
				newClassOop = (classHeader & 4294967292U) + bytesToShift;
				longAtput(oop - 4, newClassOop | (classHeader & 3));
			}
		}
		/* begin objectAfter: */
		;
		if (((longAt(oop)) & 3) == 2) {
			sz = (longAt(oop)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header1 = longAt(oop);
			if ((header1 & 3) == 0) {
				sz = (longAt(oop - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header1 & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(oop + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		oop = (oop + sz) + extra2;
	}
}

void adjustFieldsAndClassOfby(int oop, int offsetBytes) {
    int newClassOop;
    int fieldAddr;
    int fieldOop;
    int classHeader;

	fieldAddr = oop + (lastPointerOf(oop));
	while (fieldAddr > oop) {
		fieldOop = longAt(fieldAddr);
		if (!((fieldOop & 1))) {
			longAtput(fieldAddr, fieldOop + offsetBytes);
		}
		fieldAddr -= 4;
	}
	if (((longAt(oop)) & 3) != 3) {
		classHeader = longAt(oop - 4);
		newClassOop = (classHeader & 4294967292U) + offsetBytes;
		longAtput(oop - 4, newClassOop | (classHeader & 3));
	}
}

int affectedBottom(void) {
	return affectedB;
}

int affectedLeft(void) {
	return affectedL;
}

int affectedRight(void) {
	return affectedR;
}

int affectedTop(void) {
	return affectedT;
}

void allAccessibleObjectsOkay(void) {
    int oop;
    int obj;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header;
    int extra2;
    int type1;
    int extra11;

	/* begin firstAccessibleObject */
	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	obj = chunk + extra;
	while (obj < endOfMemory) {
		if (!(((longAt(obj)) & 3) == 2)) {
			oop = obj;
			goto l2;
		}
		/* begin objectAfter: */
		;
		if (((longAt(obj)) & 3) == 2) {
			sz = (longAt(obj)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header = longAt(obj);
			if ((header & 3) == 0) {
				sz = (longAt(obj - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(obj + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		obj = (obj + sz) + extra2;
	}
	error("heap is empty");
l2:	/* end firstAccessibleObject */;
	while (!(oop == null)) {
		okayFields(oop);
		oop = accessibleObjectAfter(oop);
	}
}

int allYoungand(int array1, int array2) {
    int fieldOffset;
    int methodHeader;
    int sz;
    int fmt;
    int header;
    int header1;
    int type;

	if (array1 < youngStart) {
		return 0;
	}
	if (array2 < youngStart) {
		return 0;
	}
	/* begin lastPointerOf: */
	header = longAt(array1);
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			fieldOffset = (6 + (fetchStackPointerOf(array1))) * 4;
			goto l1;
		}
		/* begin sizeBitsOfSafe: */
		header1 = longAt(array1);
		/* begin rightType: */
		if ((header1 & 252) == 0) {
			type = 0;
			goto l2;
		} else {
			if ((header1 & 126976) == 0) {
				type = 1;
				goto l2;
			} else {
				type = 3;
				goto l2;
			}
		}
	l2:	/* end rightType: */;
		if (type == 0) {
			sz = (longAt(array1 - 8)) & 4294967292U;
			goto l3;
		} else {
			sz = header1 & 252;
			goto l3;
		}
	l3:	/* end sizeBitsOfSafe: */;
		fieldOffset = sz - 4;
		goto l1;
	}
	if (fmt < 12) {
		fieldOffset = 0;
		goto l1;
	}
	methodHeader = longAt(array1 + 4);
	fieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
l1:	/* end lastPointerOf: */;
	while (fieldOffset >= 4) {
		if ((longAt(array1 + fieldOffset)) < youngStart) {
			return 0;
		}
		if ((longAt(array2 + fieldOffset)) < youngStart) {
			return 0;
		}
		fieldOffset -= 4;
	}
	return 1;
}

int allocateheaderSizeh1h2h3doFillwith(int byteSize, int hdrSize, int baseHeader, int classOop, int extendedSize, int doFill, int fillWord) {
    int i;
    int newObj;
    int remappedClassOop;
    int end;
    int oop;
    int newFreeSize;
    int enoughSpace;
    int newChunk;
    int minFree;

	if (hdrSize > 1) {
		/* begin pushRemappableOop: */
		remapBuffer[remapBufferCount += 1] = classOop;
	}
	/* begin allocateChunk: */
	if (allocationCount >= allocationsBetweenGCs) {
		incrementalGC();
	}
	/* begin sufficientSpaceToAllocate: */
	minFree = (lowSpaceThreshold + (byteSize + ((hdrSize - 1) * 4))) + 4;
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
		enoughSpace = 1;
		goto l1;
	} else {
		enoughSpace = sufficientSpaceAfterGC(minFree);
		goto l1;
	}
l1:	/* end sufficientSpaceToAllocate: */;
	if (!(enoughSpace)) {
		signalLowSpace = 1;
		lowSpaceThreshold = 0;
		interruptCheckCounter = 0;
	}
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) < (((unsigned ) ((byteSize + ((hdrSize - 1) * 4)) + 4)))) {
		error("out of memory");
	}
	newFreeSize = ((longAt(freeBlock)) & 4294967292U) - (byteSize + ((hdrSize - 1) * 4));
	newChunk = freeBlock;
	freeBlock += byteSize + ((hdrSize - 1) * 4);
	/* begin setSizeOfFree:to: */
	longAtput(freeBlock, (newFreeSize & 4294967292U) | 2);
	allocationCount += 1;
	newObj = newChunk;
	if (hdrSize > 1) {
		/* begin popRemappableOop */
		oop = remapBuffer[remapBufferCount];
		remapBufferCount -= 1;
		remappedClassOop = oop;
	}
	if (hdrSize == 3) {
		longAtput(newObj, extendedSize | 0);
		longAtput(newObj + 4, remappedClassOop | 0);
		longAtput(newObj + 8, baseHeader | 0);
		newObj += 8;
	}
	if (hdrSize == 2) {
		longAtput(newObj, remappedClassOop | 1);
		longAtput(newObj + 4, baseHeader | 1);
		newObj += 4;
	}
	if (hdrSize == 1) {
		longAtput(newObj, baseHeader | 3);
	}
	if (doFill) {
		end = newObj + byteSize;
		i = newObj + 4;
		while (i < end) {
			longAtput(i, fillWord);
			i += 4;
		}
	}
	;
	return newObj;
}

int allocateChunk(int byteSize) {
    int newFreeSize;
    int enoughSpace;
    int newChunk;
    int minFree;

	if (allocationCount >= allocationsBetweenGCs) {
		incrementalGC();
	}
	/* begin sufficientSpaceToAllocate: */
	minFree = (lowSpaceThreshold + byteSize) + 4;
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
		enoughSpace = 1;
		goto l1;
	} else {
		enoughSpace = sufficientSpaceAfterGC(minFree);
		goto l1;
	}
l1:	/* end sufficientSpaceToAllocate: */;
	if (!(enoughSpace)) {
		signalLowSpace = 1;
		lowSpaceThreshold = 0;
		interruptCheckCounter = 0;
	}
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) < (((unsigned ) (byteSize + 4)))) {
		error("out of memory");
	}
	newFreeSize = ((longAt(freeBlock)) & 4294967292U) - byteSize;
	newChunk = freeBlock;
	freeBlock += byteSize;
	/* begin setSizeOfFree:to: */
	longAtput(freeBlock, (newFreeSize & 4294967292U) | 2);
	allocationCount += 1;
	return newChunk;
}

int allocateOrRecycleContext(void) {
    int cntxt;

	if (freeContexts != 1) {
		cntxt = freeContexts;
		freeContexts = longAt(((((char *) cntxt)) + 4) + (0 << 2));
		return cntxt;
	}
	cntxt = instantiateContextsizeInBytes(longAt(((((char *) specialObjectsOop)) + 4) + (10 << 2)), 156);
	longAtput(((((char *) cntxt)) + 4) + (4 << 2), nilObj);
	return cntxt;
}

int alphaBlendwith(int sourceWord, int destinationWord) {
    int shift;
    int alpha;
    int i;
    int blend;
    int unAlpha;
    int result;
    int colorMask;

	alpha = ((unsigned) sourceWord) >> 24;
	unAlpha = 255 - alpha;
	colorMask = 255;
	result = 0;
	for (i = 1; i <= 3; i += 1) {
		shift = (i - 1) * 8;
		blend = ((((((((unsigned) sourceWord) >> shift) & colorMask) * alpha) + (((((unsigned) destinationWord) >> shift) & colorMask) * unAlpha)) + 254) / 255) & colorMask;
		result = result | (blend << shift);
	}
	return result;
}

int alphaBlendConstwith(int sourceWord, int destinationWord) {
	return alphaBlendConstwithpaintMode(sourceWord, destinationWord, 0);
}

int alphaBlendConstwithpaintMode(int sourceWord, int destinationWord, int paintMode) {
    int j;
    int destPixVal;
    int pixMask;
    int destShifted;
    int sourceShifted;
    int sourcePixVal;
    int pixBlend;
    int shift;
    int blend;
    int maskShifted;
    int bitsPerColor;
    int i;
    int unAlpha;
    int result;
    int rgbMask;

	if (destPixSize < 16) {
		return destinationWord;
	}
	unAlpha = 255 - sourceAlpha;
	pixMask = maskTable[destPixSize];
	if (destPixSize == 16) {
		bitsPerColor = 5;
	} else {
		bitsPerColor = 8;
	}
	rgbMask = (1 << bitsPerColor) - 1;
	maskShifted = destMask;
	destShifted = destinationWord;
	sourceShifted = sourceWord;
	result = destinationWord;
	for (j = 1; j <= pixPerWord; j += 1) {
		sourcePixVal = sourceShifted & pixMask;
		if (!(((maskShifted & pixMask) == 0) || (paintMode && (sourcePixVal == 0)))) {
			destPixVal = destShifted & pixMask;
			pixBlend = 0;
			for (i = 1; i <= 3; i += 1) {
				shift = (i - 1) * bitsPerColor;
				blend = ((((((((unsigned) sourcePixVal) >> shift) & rgbMask) * sourceAlpha) + (((((unsigned) destPixVal) >> shift) & rgbMask) * unAlpha)) + 254) / 255) & rgbMask;
				pixBlend = pixBlend | (blend << shift);
			}
			if (destPixSize == 16) {
				result = (result & (~(pixMask << ((j - 1) * 16)))) | (pixBlend << ((j - 1) * 16));
			} else {
				result = pixBlend;
			}
		}
		maskShifted = ((unsigned) maskShifted) >> destPixSize;
		sourceShifted = ((unsigned) sourceShifted) >> destPixSize;
		destShifted = ((unsigned) destShifted) >> destPixSize;
	}
	return result;
}

int alphaBlendScaledwith(int sourceWord, int destinationWord) {
    int dstMask;
    int srcMask;
    int b;
    int a;
    int unAlpha;
    int g;
    int r;

	unAlpha = 255 - (((unsigned) sourceWord) >> 24);
	dstMask = destinationWord;
	srcMask = sourceWord;
	b = (((unsigned) ((dstMask & 255) * unAlpha)) >> 8) + (srcMask & 255);
	if (b > 255) {
		b = 255;
	}
	dstMask = ((unsigned) dstMask) >> 8;
	srcMask = ((unsigned) srcMask) >> 8;
	g = (((unsigned) ((dstMask & 255) * unAlpha)) >> 8) + (srcMask & 255);
	if (g > 255) {
		g = 255;
	}
	dstMask = ((unsigned) dstMask) >> 8;
	srcMask = ((unsigned) srcMask) >> 8;
	r = (((unsigned) ((dstMask & 255) * unAlpha)) >> 8) + (srcMask & 255);
	if (r > 255) {
		r = 255;
	}
	dstMask = ((unsigned) dstMask) >> 8;
	srcMask = ((unsigned) srcMask) >> 8;
	a = (((unsigned) ((dstMask & 255) * unAlpha)) >> 8) + (srcMask & 255);
	if (a > 255) {
		a = 255;
	}
	return (((((a << 8) + r) << 8) + g) << 8) + b;
}

int alphaPaintConstwith(int sourceWord, int destinationWord) {
	if (sourceWord == 0) {
		return destinationWord;
	}
	return alphaBlendConstwithpaintMode(sourceWord, destinationWord, 1);
}

void alphaSourceBlendBits16(void) {
    int deltaX;
    int deltaY;
    int srcIndex;
    int dstIndex;
    int sourceWord;
    int srcAlpha;
    int destWord;
    int srcY;
    int dstY;
    int dstMask;
    int srcShift;
    int adjust;
    int mask;
    int srcPix;
    int destPix;
    int d;
    int mask3;
    int srcPix1;
    int destPix1;
    int d1;
    int mask4;
    int srcPix2;
    int destPix2;
    int d2;

	deltaY = bbH + 1;
	srcY = sy;
	dstY = dy;
	if ((dx & 1) == 0) {
		mask1 = 65535;
		srcShift = 16;
		adjust = 0;
	} else {
		mask1 = 4294901760U;
		srcShift = 0;
		adjust = 252645135;
	}
	if ((dy & 1) == 0) {
		adjust = adjust ^ 252645135;
	}
	while ((deltaY -= 1) != 0) {
		adjust = adjust ^ 252645135;
		srcIndex = (sourceBits + 4) + (((srcY * sourceRaster) + sx) * 4);
		dstIndex = (destBits + 4) + (((dstY * destRaster) + (((int) dx >> 1))) * 4);
		deltaX = bbW + 1;
		dstMask = mask1;
		while ((deltaX -= 1) != 0) {
			sourceWord = ((longAt(srcIndex)) & (~adjust)) + adjust;
			srcAlpha = ((unsigned) sourceWord) >> 24;
			if (srcAlpha == 240) {
				/* begin rgbMap:from:to: */
				if ((d = 5 - 8) > 0) {
					mask = (1 << 8) - 1;
					srcPix = sourceWord << d;
					mask = mask << d;
					destPix = srcPix & mask;
					mask = mask << 5;
					srcPix = srcPix << d;
					sourceWord = (destPix + (srcPix & mask)) + ((srcPix << d) & (mask << 5));
					goto l1;
				} else {
					if (d == 0) {
						sourceWord = sourceWord;
						goto l1;
					}
					if (sourceWord == 0) {
						sourceWord = sourceWord;
						goto l1;
					}
					d = 8 - 5;
					mask = (1 << 5) - 1;
					srcPix = ((unsigned) sourceWord) >> d;
					destPix = srcPix & mask;
					mask = mask << 5;
					srcPix = ((unsigned) srcPix) >> d;
					destPix = (destPix + (srcPix & mask)) + ((((unsigned) srcPix) >> d) & (mask << 5));
					if (destPix == 0) {
						sourceWord = 1;
						goto l1;
					}
					sourceWord = destPix;
					goto l1;
				}
			l1:	/* end rgbMap:from:to: */;
				sourceWord = sourceWord << srcShift;
				destWord = longAt(dstIndex);
				destWord = destWord & dstMask;
				longAtput(dstIndex, sourceWord | destWord);
			} else {
				if (srcAlpha <= 15) {
					null;
				} else {
					destWord = longAt(dstIndex);
					destWord = destWord & (~dstMask);
					destWord = ((unsigned) destWord) >> srcShift;
					/* begin rgbMap:from:to: */
					if ((d1 = 8 - 5) > 0) {
						mask3 = (1 << 5) - 1;
						srcPix1 = destWord << d1;
						mask3 = mask3 << d1;
						destPix1 = srcPix1 & mask3;
						mask3 = mask3 << 8;
						srcPix1 = srcPix1 << d1;
						destWord = (destPix1 + (srcPix1 & mask3)) + ((srcPix1 << d1) & (mask3 << 8));
						goto l2;
					} else {
						if (d1 == 0) {
							destWord = destWord;
							goto l2;
						}
						if (destWord == 0) {
							destWord = destWord;
							goto l2;
						}
						d1 = 5 - 8;
						mask3 = (1 << 8) - 1;
						srcPix1 = ((unsigned) destWord) >> d1;
						destPix1 = srcPix1 & mask3;
						mask3 = mask3 << 8;
						srcPix1 = ((unsigned) srcPix1) >> d1;
						destPix1 = (destPix1 + (srcPix1 & mask3)) + ((((unsigned) srcPix1) >> d1) & (mask3 << 8));
						if (destPix1 == 0) {
							destWord = 1;
							goto l2;
						}
						destWord = destPix1;
						goto l2;
					}
				l2:	/* end rgbMap:from:to: */;
					destWord = destWord | 4278190080U;
					sourceWord = alphaBlendScaledwith(sourceWord, destWord);
					/* begin rgbMap:from:to: */
					if ((d2 = 5 - 8) > 0) {
						mask4 = (1 << 8) - 1;
						srcPix2 = sourceWord << d2;
						mask4 = mask4 << d2;
						destPix2 = srcPix2 & mask4;
						mask4 = mask4 << 5;
						srcPix2 = srcPix2 << d2;
						sourceWord = (destPix2 + (srcPix2 & mask4)) + ((srcPix2 << d2) & (mask4 << 5));
						goto l3;
					} else {
						if (d2 == 0) {
							sourceWord = sourceWord;
							goto l3;
						}
						if (sourceWord == 0) {
							sourceWord = sourceWord;
							goto l3;
						}
						d2 = 8 - 5;
						mask4 = (1 << 5) - 1;
						srcPix2 = ((unsigned) sourceWord) >> d2;
						destPix2 = srcPix2 & mask4;
						mask4 = mask4 << 5;
						srcPix2 = ((unsigned) srcPix2) >> d2;
						destPix2 = (destPix2 + (srcPix2 & mask4)) + ((((unsigned) srcPix2) >> d2) & (mask4 << 5));
						if (destPix2 == 0) {
							sourceWord = 1;
							goto l3;
						}
						sourceWord = destPix2;
						goto l3;
					}
				l3:	/* end rgbMap:from:to: */;
					sourceWord = sourceWord << srcShift;
					destWord = longAt(dstIndex);
					destWord = destWord & dstMask;
					longAtput(dstIndex, sourceWord | destWord);
				}
			}
			srcIndex += 4;
			if (srcShift == 0) {
				dstIndex += 4;
			}
			srcShift = srcShift ^ 16;
			dstMask = ~dstMask;
			adjust = adjust ^ 252645135;
		}
		srcY += 1;
		dstY += 1;
	}
}

void alphaSourceBlendBits32(void) {
    register int deltaX;
    int deltaY;
    register int srcIndex;
    register int dstIndex;
    register int sourceWord;
    int srcAlpha;
    int destWord;
    int srcY;
    int dstY;

	deltaY = bbH + 1;
	srcY = sy;
	dstY = dy;
	while ((deltaY -= 1) != 0) {
		srcIndex = (sourceBits + 4) + (((srcY * sourceRaster) + sx) * 4);
		dstIndex = (destBits + 4) + (((dstY * destRaster) + dx) * 4);
		deltaX = bbW + 1;
		while ((deltaX -= 1) != 0) {
			sourceWord = longAt(srcIndex);
			srcAlpha = ((unsigned) sourceWord) >> 24;
			if (srcAlpha == 255) {
				longAtput(dstIndex, sourceWord);
				srcIndex += 4;
				dstIndex += 4;
				while (((deltaX -= 1) != 0) && ((((unsigned) (sourceWord = longAt(srcIndex))) >> 24) == 255)) {
					longAtput(dstIndex, sourceWord);
					srcIndex += 4;
					dstIndex += 4;
				}
				deltaX += 1;
			} else {
				if (srcAlpha == 0) {
					srcIndex += 4;
					dstIndex += 4;
					while (((deltaX -= 1) != 0) && ((((unsigned) (sourceWord = longAt(srcIndex))) >> 24) == 0)) {
						srcIndex += 4;
						dstIndex += 4;
					}
					deltaX += 1;
				} else {
					destWord = longAt(dstIndex);
					destWord = alphaBlendScaledwith(sourceWord, destWord);
					longAtput(dstIndex, destWord);
					srcIndex += 4;
					dstIndex += 4;
				}
			}
		}
		srcY += 1;
		dstY += 1;
	}
}

void alphaSourceBlendBits8(void) {
    int deltaX;
    int deltaY;
    int srcIndex;
    int dstIndex;
    int sourceWord;
    int srcAlpha;
    int destWord;
    int srcY;
    int dstY;
    int dstMask;
    int srcShift;
    int adjust;
    unsigned int *mappingTable;
    int mask;
    int srcPix;
    int destPix;
    int d;

	mappingTable = default8To32Table();
	deltaY = bbH + 1;
	srcY = sy;
	dstY = dy;
	mask1 = 24 - ((dx & 3) * 8);
	mask2 = 4294967295U ^ (255 << mask1);
	if ((dx & 1) == 0) {
		adjust = 0;
	} else {
		adjust = 522133279;
	}
	if ((dy & 1) == 0) {
		adjust = adjust ^ 522133279;
	}
	while ((deltaY -= 1) != 0) {
		adjust = adjust ^ 522133279;
		srcIndex = (sourceBits + 4) + (((srcY * sourceRaster) + sx) * 4);
		dstIndex = (destBits + 4) + (((dstY * destRaster) + (((int) dx >> 2))) * 4);
		deltaX = bbW + 1;
		srcShift = mask1;
		dstMask = mask2;
		while ((deltaX -= 1) != 0) {
			sourceWord = ((longAt(srcIndex)) & (~adjust)) + adjust;
			srcAlpha = ((unsigned) sourceWord) >> 24;
			if (srcAlpha > 31) {
				if (srcAlpha < 224) {
					destWord = longAt(dstIndex);
					destWord = destWord & (~dstMask);
					destWord = ((unsigned) destWord) >> srcShift;
					destWord = mappingTable[destWord];
					sourceWord = alphaBlendScaledwith(sourceWord, destWord);
				}
				/* begin rgbMap:from:to: */
				if ((d = cmBitsPerColor - 8) > 0) {
					mask = (1 << 8) - 1;
					srcPix = sourceWord << d;
					mask = mask << d;
					destPix = srcPix & mask;
					mask = mask << cmBitsPerColor;
					srcPix = srcPix << d;
					sourceWord = (destPix + (srcPix & mask)) + ((srcPix << d) & (mask << cmBitsPerColor));
					goto l1;
				} else {
					if (d == 0) {
						sourceWord = sourceWord;
						goto l1;
					}
					if (sourceWord == 0) {
						sourceWord = sourceWord;
						goto l1;
					}
					d = 8 - cmBitsPerColor;
					mask = (1 << cmBitsPerColor) - 1;
					srcPix = ((unsigned) sourceWord) >> d;
					destPix = srcPix & mask;
					mask = mask << cmBitsPerColor;
					srcPix = ((unsigned) srcPix) >> d;
					destPix = (destPix + (srcPix & mask)) + ((((unsigned) srcPix) >> d) & (mask << cmBitsPerColor));
					if (destPix == 0) {
						sourceWord = 1;
						goto l1;
					}
					sourceWord = destPix;
					goto l1;
				}
			l1:	/* end rgbMap:from:to: */;
				sourceWord = longAt(((((char *) colorMap)) + 4) + (sourceWord << 2));
				sourceWord = sourceWord << srcShift;
				destWord = longAt(dstIndex);
				destWord = destWord & dstMask;
				longAtput(dstIndex, sourceWord | destWord);
			}
			srcIndex += 4;
			if (srcShift == 0) {
				dstIndex += 4;
				srcShift = 24;
				dstMask = 16777215;
			} else {
				srcShift -= 8;
				dstMask = (((unsigned) dstMask) >> 8) | 4278190080U;
			}
			adjust = adjust ^ 522133279;
		}
		srcY += 1;
		dstY += 1;
	}
}

int areIntegersand(int oop1, int oop2) {
	return ((oop1 & oop2) & 1) != 0;
}

int argCount(void) {
	return argumentCount;
}

int argumentCountOf(int methodPointer) {
	return (((unsigned) (longAt(((((char *) methodPointer)) + 4) + (0 << 2)))) >> 25) & 15;
}

int argumentCountOfBlock(int blockPointer) {
    int argCount;

	argCount = longAt(((((char *) blockPointer)) + 4) + (3 << 2));
	if ((argCount & 1)) {
		return (argCount >> 1);
	} else {
		successFlag = 0;
		return 0;
	}
}

void * arrayValueOf(int arrayOop) {
	if ((!((arrayOop & 1))) && (isWordsOrBytes(arrayOop))) {
		return (void *) (arrayOop + 4);
	}
	successFlag = 0;
}

int asciiDirectoryDelimiter(void) {
	return dir_Delimitor();
}

int asciiOfCharacter(int characterObj) {
	if (successFlag) {
		return longAt(((((char *) characterObj)) + 4) + (0 << 2));
	} else {
		return 1;
	}
}

int baseHeader(int oop) {
	return longAt(oop);
}

void beRootIfOld(int oop) {
    int header;

	if ((oop < youngStart) && (!((oop & 1)))) {
		header = longAt(oop);
		if ((header & 1073741824) == 0) {
			if (rootTableCount < 500) {
				rootTableCount += 1;
				rootTable[rootTableCount] = oop;
				longAtput(oop, header | 1073741824);
			}
		}
	}
}

void beRootWhileForwarding(int oop) {
    int fwdBlock;
    int forwarding;
    int header;
    int newHeader;

	header = longAt(oop);
	if ((header & 2147483648U) != 0) {
		forwarding = 1;
		fwdBlock = (header & 2147483644) << 1;
		;
		header = longAt(fwdBlock + 4);
	} else {
		forwarding = 0;
	}
	if ((header & 1073741824) == 0) {
		if (rootTableCount < 500) {
			rootTableCount += 1;
			rootTable[rootTableCount] = oop;
			newHeader = header | 1073741824;
			if (forwarding) {
				longAtput(fwdBlock + 4, newHeader);
			} else {
				longAtput(oop, newHeader);
			}
		}
	}
}

int becomewithtwoWay(int array1, int array2, int twoWayFlag) {
    int fwdBlock;
    int oop1;
    int fwdBlock1;
    int fwdHeader;
    int fieldOffset;
    int oop11;
    int oop2;
    int hdr1;
    int hdr2;
    int fwdBlock2;
    int fwdHeader2;
    int fwdBlock11;
    int fwdHeader1;
    int methodHeader;
    int sz;
    int fmt;
    int header;
    int header1;
    int type;

	if (!((fetchClassOf(array1)) == (longAt(((((char *) specialObjectsOop)) + 4) + (7 << 2))))) {
		return 0;
	}
	if (!((fetchClassOf(array2)) == (longAt(((((char *) specialObjectsOop)) + 4) + (7 << 2))))) {
		return 0;
	}
	if (!((lastPointerOf(array1)) == (lastPointerOf(array2)))) {
		return 0;
	}
	if (!(containOnlyOopsand(array1, array2))) {
		return 0;
	}
	if (!(prepareForwardingTableForBecomingwithtwoWay(array1, array2, twoWayFlag))) {
		return 0;
	}
	if (allYoungand(array1, array2)) {
		mapPointersInObjectsFromto(youngStart, endOfMemory);
	} else {
		mapPointersInObjectsFromto(startOfMemory(), endOfMemory);
	}
	if (twoWayFlag) {
		/* begin restoreHeadersAfterBecoming:with: */
		/* begin lastPointerOf: */
		header = longAt(array1);
		fmt = (((unsigned) header) >> 8) & 15;
		if (fmt <= 4) {
			if ((fmt == 3) && (isContextHeader(header))) {
				fieldOffset = (6 + (fetchStackPointerOf(array1))) * 4;
				goto l1;
			}
			/* begin sizeBitsOfSafe: */
			header1 = longAt(array1);
			/* begin rightType: */
			if ((header1 & 252) == 0) {
				type = 0;
				goto l2;
			} else {
				if ((header1 & 126976) == 0) {
					type = 1;
					goto l2;
				} else {
					type = 3;
					goto l2;
				}
			}
		l2:	/* end rightType: */;
			if (type == 0) {
				sz = (longAt(array1 - 8)) & 4294967292U;
				goto l3;
			} else {
				sz = header1 & 252;
				goto l3;
			}
		l3:	/* end sizeBitsOfSafe: */;
			fieldOffset = sz - 4;
			goto l1;
		}
		if (fmt < 12) {
			fieldOffset = 0;
			goto l1;
		}
		methodHeader = longAt(array1 + 4);
		fieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
	l1:	/* end lastPointerOf: */;
		while (fieldOffset >= 4) {
			oop11 = longAt(array1 + fieldOffset);
			oop2 = longAt(array2 + fieldOffset);
			/* begin restoreHeaderOf: */
			fwdHeader2 = longAt(oop11);
			fwdBlock2 = (fwdHeader2 & 2147483644) << 1;
			;
			longAtput(oop11, longAt(fwdBlock2 + 4));
			/* begin restoreHeaderOf: */
			fwdHeader1 = longAt(oop2);
			fwdBlock11 = (fwdHeader1 & 2147483644) << 1;
			;
			longAtput(oop2, longAt(fwdBlock11 + 4));
			/* begin exchangeHashBits:with: */
			hdr1 = longAt(oop11);
			hdr2 = longAt(oop2);
			longAtput(oop11, (hdr1 & 3758227455U) | (hdr2 & 536739840));
			longAtput(oop2, (hdr2 & 3758227455U) | (hdr1 & 536739840));
			fieldOffset -= 4;
		}
	} else {
		/* begin restoreHeadersAfterForwardBecome */
		fwdBlock = ((endOfMemory + 4) + 7) & 4294967288U;
		fwdBlock += 16;
		while (fwdBlock <= fwdTableNext) {
			oop1 = longAt(fwdBlock + 8);
			/* begin restoreHeaderOf: */
			fwdHeader = longAt(oop1);
			fwdBlock1 = (fwdHeader & 2147483644) << 1;
			;
			longAtput(oop1, longAt(fwdBlock1 + 4));
			fwdBlock += 16;
		}
	}
	initializeMemoryFirstFree(freeBlock);
	return 1;
}

int bitAndwith(int sourceWord, int destinationWord) {
	return sourceWord & destinationWord;
}

int bitAndInvertwith(int sourceWord, int destinationWord) {
	return sourceWord & (~destinationWord);
}

int bitInvertAndwith(int sourceWord, int destinationWord) {
	return (~sourceWord) & destinationWord;
}

int bitInvertAndInvertwith(int sourceWord, int destinationWord) {
	return (~sourceWord) & (~destinationWord);
}

int bitInvertDestinationwith(int sourceWord, int destinationWord) {
	return ~destinationWord;
}

int bitInvertOrwith(int sourceWord, int destinationWord) {
	return (~sourceWord) | destinationWord;
}

int bitInvertOrInvertwith(int sourceWord, int destinationWord) {
	return (~sourceWord) | (~destinationWord);
}

int bitInvertSourcewith(int sourceWord, int destinationWord) {
	return ~sourceWord;
}

int bitInvertXorwith(int sourceWord, int destinationWord) {
	return (~sourceWord) ^ destinationWord;
}

int bitOrwith(int sourceWord, int destinationWord) {
	return sourceWord | destinationWord;
}

int bitOrInvertwith(int sourceWord, int destinationWord) {
	return sourceWord | (~destinationWord);
}

int bitXorwith(int sourceWord, int destinationWord) {
	return sourceWord ^ destinationWord;
}

int booleanValueOf(int obj) {
	if (obj == trueObj) {
		return 1;
	}
	if (obj == falseObj) {
		return 0;
	}
	successFlag = 0;
	return null;
}

int byteLengthOf(int oop) {
    int sz;
    int header;
    int fmt;

	header = longAt(oop);
	if ((header & 3) == 0) {
		sz = (longAt(oop - 8)) & 4294967292U;
	} else {
		sz = header & 252;
	}
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt < 8) {
		return sz - 4;
	} else {
		return (sz - 4) - (fmt & 3);
	}
}

void byteSwapByteObjects(void) {
    int methodHeader;
    int wordAddr;
    int oop;
    int fmt;
    int stopAddr;
    int addr;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header;
    int extra2;
    int type1;
    int extra11;

	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	oop = chunk + extra;
	while (oop < endOfMemory) {
		if (!(((longAt(oop)) & 3) == 2)) {
			fmt = (((unsigned) (longAt(oop))) >> 8) & 15;
			if (fmt >= 8) {
				wordAddr = oop + 4;
				if (fmt >= 12) {
					methodHeader = longAt(oop + 4);
					wordAddr = (wordAddr + 4) + (((((unsigned) methodHeader) >> 10) & 255) * 4);
				}
				/* begin reverseBytesFrom:to: */
				stopAddr = oop + (sizeBitsOf(oop));
				addr = wordAddr;
				while (addr < stopAddr) {
					longAtput(addr, ((((((unsigned) (longAt(addr)) >> 24)) & 255) + ((((unsigned) (longAt(addr)) >> 8)) & 65280)) + ((((unsigned) (longAt(addr)) << 8)) & 16711680)) + ((((unsigned) (longAt(addr)) << 24)) & 4278190080U));
					addr += 4;
				}
			}
		}
		/* begin objectAfter: */
		;
		if (((longAt(oop)) & 3) == 2) {
			sz = (longAt(oop)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header = longAt(oop);
			if ((header & 3) == 0) {
				sz = (longAt(oop - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(oop + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		oop = (oop + sz) + extra2;
	}
}

int byteSwapped(int w) {
	return ((((((unsigned) w >> 24)) & 255) + ((((unsigned) w >> 8)) & 65280)) + ((((unsigned) w << 8)) & 16711680)) + ((((unsigned) w << 24)) & 4278190080U);
}

int caller(void) {
	return longAt(((((char *) activeContext)) + 4) + (0 << 2));
}

int characterForAscii(int ascii) {
	return longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (24 << 2))))) + 4) + (ascii << 2));
}

void checkAddress(int byteAddress) {
	if (byteAddress < (startOfMemory())) {
		error("bad address: negative");
	}
	if (byteAddress >= memoryLimit) {
		error("bad address: past end of heap");
	}
}

void checkBooleanResult(int result) {
    int sp;
    int sp1;

	if (successFlag) {
		/* begin pushBool: */
		if (result) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void checkForInterrupts(void) {
    int sema;
    int i;
    int now;
    int index;

	interruptCheckCounter = 1000;
	now = (ioMSecs()) & 536870911;
	if (now < lastTick) {
		nextPollTick = now + (nextPollTick - lastTick);
		if (nextWakeupTick != 0) {
			nextWakeupTick = now + (nextWakeupTick - lastTick);
		}
	}
	lastTick = now;
	if (signalLowSpace) {
		signalLowSpace = 0;
		sema = longAt(((((char *) specialObjectsOop)) + 4) + (17 << 2));
		if (!(sema == nilObj)) {
			synchronousSignal(sema);
		}
	}
	if (now >= nextPollTick) {
		ioProcessEvents();
		nextPollTick = now + 500;
	}
	if (interruptPending) {
		interruptPending = 0;
		sema = longAt(((((char *) specialObjectsOop)) + 4) + (30 << 2));
		if (!(sema == nilObj)) {
			synchronousSignal(sema);
		}
	}
	if ((nextWakeupTick != 0) && (now >= nextWakeupTick)) {
		nextWakeupTick = 0;
		sema = longAt(((((char *) specialObjectsOop)) + 4) + (29 << 2));
		if (!(sema == nilObj)) {
			synchronousSignal(sema);
		}
	}
	if (pendingFinalizationSignals > 0) {
		sema = longAt(((((char *) specialObjectsOop)) + 4) + (41 << 2));
		if ((fetchClassOf(sema)) == (longAt(((((char *) specialObjectsOop)) + 4) + (18 << 2)))) {
			synchronousSignal(sema);
		}
		pendingFinalizationSignals = 0;
	}
	if (semaphoresToSignalCount > 0) {
		for (i = 1; i <= semaphoresToSignalCount; i += 1) {
			index = semaphoresToSignal[i];
			sema = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (38 << 2))))) + 4) + ((index - 1) << 2));
			if ((fetchClassOf(sema)) == (longAt(((((char *) specialObjectsOop)) + 4) + (18 << 2)))) {
				synchronousSignal(sema);
			}
		}
		semaphoresToSignalCount = 0;
	}
}

int checkImageVersionFrom(int f) {
    int version;

	sqImageFileSeek(f, 0);
	version = getLongFromFileswap(f, 0);
	if ((version == 6502) || (version == 6504)) {
		return 0;
	}
	sqImageFileSeek(f, 0);
	version = getLongFromFileswap(f, 1);
	if ((version == 6502) || (version == 6504)) {
		return 1;
	}
	sqImageFileSeek(f, 512);
	version = getLongFromFileswap(f, 0);
	if ((version == 6502) || (version == 6504)) {
		return 0;
	}
	sqImageFileSeek(f, 512);
	version = getLongFromFileswap(f, 1);
	if ((version == 6502) || (version == 6504)) {
		return 1;
	}
	print("Incompatible image version.");
	ioPutChar(13);
	ioExit();
}

void checkSourceOverlap(void) {
    int t;

	if ((sourceForm == destForm) && (dy >= sy)) {
		if (dy > sy) {
			vDir = -1;
			sy = (sy + bbH) - 1;
			dy = (dy + bbH) - 1;
		} else {
			if ((dy == sy) && (dx > sx)) {
				hDir = -1;
				sx = (sx + bbW) - 1;
				dx = (dx + bbW) - 1;
				if (nWords > 1) {
					t = mask1;
					mask1 = mask2;
					mask2 = t;
				}
			}
		}
		destIndex = (destBits + 4) + (((dy * destRaster) + (dx / pixPerWord)) * 4);
		destDelta = 4 * ((destRaster * vDir) - (nWords * hDir));
	}
}

int checkedByteAt(int byteAddress) {
	/* begin checkAddress: */
	if (byteAddress < (startOfMemory())) {
		error("bad address: negative");
	}
	if (byteAddress >= memoryLimit) {
		error("bad address: past end of heap");
	}
	return byteAt(byteAddress);
}

void checkedByteAtput(int byteAddress, int byte) {
	/* begin checkAddress: */
	if (byteAddress < (startOfMemory())) {
		error("bad address: negative");
	}
	if (byteAddress >= memoryLimit) {
		error("bad address: past end of heap");
	}
	byteAtput(byteAddress, byte);
}

int checkedIntegerValueOf(int intOop) {
	if ((intOop & 1)) {
		return (intOop >> 1);
	} else {
		successFlag = 0;
		return 0;
	}
}

int checkedLongAt(int byteAddress) {
	/* begin checkAddress: */
	if (byteAddress < (startOfMemory())) {
		error("bad address: negative");
	}
	if (byteAddress >= memoryLimit) {
		error("bad address: past end of heap");
	}
	/* begin checkAddress: */
	if ((byteAddress + 3) < (startOfMemory())) {
		error("bad address: negative");
	}
	if ((byteAddress + 3) >= memoryLimit) {
		error("bad address: past end of heap");
	}
	return longAt(byteAddress);
}

void checkedLongAtput(int byteAddress, int a32BitInteger) {
	/* begin checkAddress: */
	if (byteAddress < (startOfMemory())) {
		error("bad address: negative");
	}
	if (byteAddress >= memoryLimit) {
		error("bad address: past end of heap");
	}
	/* begin checkAddress: */
	if ((byteAddress + 3) < (startOfMemory())) {
		error("bad address: negative");
	}
	if ((byteAddress + 3) >= memoryLimit) {
		error("bad address: past end of heap");
	}
	longAtput(byteAddress, a32BitInteger);
}

int chunkFromOop(int oop) {
    int extra;
    int type;
    int extra1;

	/* begin extraHeaderBytes: */
	type = (longAt(oop)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	return oop - extra;
}

int classHeader(int oop) {
	return longAt(oop - 4);
}

void cleanUpContexts(void) {
    int sz;
    int i;
    int header;
    int oop;
    int fmt;
    int header1;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz1;
    int header2;
    int extra2;
    int type1;
    int extra11;

	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	oop = chunk + extra;
	while (oop < endOfMemory) {
		if (!(((longAt(oop)) & 3) == 2)) {
			header = longAt(oop);
			fmt = (((unsigned) header) >> 8) & 15;
			if ((fmt == 3) && (isContextHeader(header))) {
				/* begin sizeBitsOf: */
				header1 = longAt(oop);
				if ((header1 & 3) == 0) {
					sz = (longAt(oop - 8)) & 4294967292U;
					goto l1;
				} else {
					sz = header1 & 252;
					goto l1;
				}
			l1:	/* end sizeBitsOf: */;
				for (i = ((lastPointerOf(oop)) + 4); i <= (sz - 4); i += 4) {
					longAtput(oop + i, nilObj);
				}
			}
		}
		/* begin objectAfter: */
		;
		if (((longAt(oop)) & 3) == 2) {
			sz1 = (longAt(oop)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header2 = longAt(oop);
			if ((header2 & 3) == 0) {
				sz1 = (longAt(oop - 8)) & 4294967292U;
				goto l2;
			} else {
				sz1 = header2 & 252;
				goto l2;
			}
		l2:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(oop + sz1)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		oop = (oop + sz1) + extra2;
	}
}

void clearRootsTable(void) {
    int oop;
    int i;

	for (i = 1; i <= rootTableCount; i += 1) {
		oop = rootTable[i];
		longAtput(oop, (longAt(oop)) & 3221225471U);
		rootTable[i] = 0;
	}
	rootTableCount = 0;
}

int clearWordwith(int source, int destination) {
	return 0;
}

int clipRange(void) {
	if (destX >= clipX) {
		sx = sourceX;
		dx = destX;
		bbW = width;
	} else {
		sx = sourceX + (clipX - destX);
		bbW = width - (clipX - destX);
		dx = clipX;
	}
	if ((dx + bbW) > (clipX + clipWidth)) {
		bbW -= (dx + bbW) - (clipX + clipWidth);
	}
	if (destY >= clipY) {
		sy = sourceY;
		dy = destY;
		bbH = height;
	} else {
		sy = (sourceY + clipY) - destY;
		bbH = height - (clipY - destY);
		dy = clipY;
	}
	if ((dy + bbH) > (clipY + clipHeight)) {
		bbH -= (dy + bbH) - (clipY + clipHeight);
	}
	if (noSource) {
		return null;
	}
	if (sx < 0) {
		dx -= sx;
		bbW += sx;
		sx = 0;
	}
	if ((sx + bbW) > srcWidth) {
		bbW -= (sx + bbW) - srcWidth;
	}
	if (sy < 0) {
		dy -= sy;
		bbH += sy;
		sy = 0;
	}
	if ((sy + bbH) > srcHeight) {
		bbH -= (sy + bbH) - srcHeight;
	}
}

int clone(int oop) {
    int hash;
    int fromIndex;
    int lastFrom;
    int bytes;
    int extraHdrBytes;
    int remappedOop;
    int toIndex;
    int newOop;
    int newChunk;
    int header;
    int type;
    int extra;
    int oop1;
    int header1;
    int newFreeSize;
    int enoughSpace;
    int newChunk1;
    int minFree;

	/* begin extraHeaderBytes: */
	type = (longAt(oop)) & 3;
	if (type > 1) {
		extra = 0;
	} else {
		if (type == 1) {
			extra = 4;
		} else {
			extra = 8;
		}
	}
	extraHdrBytes = extra;
	/* begin sizeBitsOf: */
	header1 = longAt(oop);
	if ((header1 & 3) == 0) {
		bytes = (longAt(oop - 8)) & 4294967292U;
		goto l1;
	} else {
		bytes = header1 & 252;
		goto l1;
	}
l1:	/* end sizeBitsOf: */;
	bytes += extraHdrBytes;
	/* begin pushRemappableOop: */
	remapBuffer[remapBufferCount += 1] = oop;
	/* begin allocateChunk: */
	if (allocationCount >= allocationsBetweenGCs) {
		incrementalGC();
	}
	/* begin sufficientSpaceToAllocate: */
	minFree = (lowSpaceThreshold + bytes) + 4;
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
		enoughSpace = 1;
		goto l2;
	} else {
		enoughSpace = sufficientSpaceAfterGC(minFree);
		goto l2;
	}
l2:	/* end sufficientSpaceToAllocate: */;
	if (!(enoughSpace)) {
		signalLowSpace = 1;
		lowSpaceThreshold = 0;
		interruptCheckCounter = 0;
	}
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) < (((unsigned ) (bytes + 4)))) {
		error("out of memory");
	}
	newFreeSize = ((longAt(freeBlock)) & 4294967292U) - bytes;
	newChunk1 = freeBlock;
	freeBlock += bytes;
	/* begin setSizeOfFree:to: */
	longAtput(freeBlock, (newFreeSize & 4294967292U) | 2);
	allocationCount += 1;
	newChunk = newChunk1;
	/* begin popRemappableOop */
	oop1 = remapBuffer[remapBufferCount];
	remapBufferCount -= 1;
	remappedOop = oop1;
	toIndex = newChunk - 4;
	fromIndex = (remappedOop - extraHdrBytes) - 4;
	lastFrom = fromIndex + bytes;
	while (fromIndex < lastFrom) {
		longAtput(toIndex += 4, longAt(fromIndex += 4));
	}
	newOop = newChunk + extraHdrBytes;
	/* begin newObjectHash */
	lastHash = (13849 + (27181 * lastHash)) & 65535;
	hash = lastHash;
	header = (longAt(newOop)) & 131071;
	header = header | ((hash << 17) & 536739840);
	longAtput(newOop, header);
	return newOop;
}

int commonAt(int stringy) {
    int index;
    int result;
    int rcvr;
    int atIx;
    int sp;
    int sp1;

	index = positive32BitValueOf(longAt(stackPointer - (0 * 4)));
	rcvr = longAt(stackPointer - (1 * 4));
	if (!(successFlag && (!((rcvr & 1))))) {
		return successFlag = 0;
	}
	if ((messageSelector == (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((16 * 2) << 2)))) && (lkupClass == (fetchClassOfNonInt(rcvr)))) {
		atIx = rcvr & 28;
		if (!((atCache[atIx + 1]) == rcvr)) {
			installinAtCacheatstring(rcvr, atCache, atIx, stringy);
		}
		if (successFlag) {
			result = commonVariableatcacheIndex(rcvr, index, atIx);
		}
		if (successFlag) {
			/* begin pop:thenPush: */
			longAtput(sp = stackPointer - ((2 - 1) * 4), result);
			return stackPointer = sp;
		}
	}
	successFlag = 1;
	result = stObjectat(rcvr, index);
	if (successFlag) {
		if (stringy) {
			result = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (24 << 2))))) + 4) + (((result >> 1)) << 2));
		}
		/* begin pop:thenPush: */
		longAtput(sp1 = stackPointer - ((2 - 1) * 4), result);
		return stackPointer = sp1;
	}
}

int commonAtPut(int stringy) {
    int value;
    int index;
    int rcvr;
    int atIx;
    int stSize;
    int valToPut;
    int fmt;
    int fixedFields;
    int sp;
    int sp1;

	value = longAt(stackPointer - (0 * 4));
	index = positive32BitValueOf(longAt(stackPointer - (1 * 4)));
	rcvr = longAt(stackPointer - (2 * 4));
	if (!(successFlag && (!((rcvr & 1))))) {
		return successFlag = 0;
	}
	if ((messageSelector == (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((17 * 2) << 2)))) && (lkupClass == (fetchClassOfNonInt(rcvr)))) {
		atIx = (rcvr & 28) + 32;
		if (!((atCache[atIx + 1]) == rcvr)) {
			installinAtCacheatstring(rcvr, atCache, atIx, stringy);
		}
		if (successFlag) {
			/* begin commonVariable:at:put:cacheIndex: */
			stSize = atCache[atIx + 2];
			if (((((unsigned ) index)) >= 1) && ((((unsigned ) index)) <= (((unsigned ) stSize)))) {
				fmt = atCache[atIx + 3];
				if (fmt <= 4) {
					fixedFields = atCache[atIx + 4];
					/* begin storePointer:ofObject:withValue: */
					if (rcvr < youngStart) {
						possibleRootStoreIntovalue(rcvr, value);
					}
					longAtput(((((char *) rcvr)) + 4) + (((index + fixedFields) - 1) << 2), value);
					goto l1;
				}
				if (fmt < 8) {
					valToPut = positive32BitValueOf(value);
					if (successFlag) {
						longAtput(((((char *) rcvr)) + 4) + ((index - 1) << 2), valToPut);
					}
					goto l1;
				}
				if (fmt >= 16) {
					valToPut = asciiOfCharacter(value);
					if (!(successFlag)) {
						goto l1;
					}
				} else {
					valToPut = value;
				}
				if ((valToPut & 1)) {
					byteAtput(((((char *) rcvr)) + 4) + (index - 1), (valToPut >> 1));
					goto l1;
				}
			}
			successFlag = 0;
		l1:	/* end commonVariable:at:put:cacheIndex: */;
		}
		if (successFlag) {
			/* begin pop:thenPush: */
			longAtput(sp = stackPointer - ((3 - 1) * 4), value);
			return stackPointer = sp;
		}
	}
	successFlag = 1;
	if (stringy) {
		stObjectatput(rcvr, index, asciiOfCharacter(value));
	} else {
		stObjectatput(rcvr, index, value);
	}
	if (successFlag) {
		/* begin pop:thenPush: */
		longAtput(sp1 = stackPointer - ((3 - 1) * 4), value);
		return stackPointer = sp1;
	}
}

int commonVariableatcacheIndex(int rcvr, int index, int atIx) {
    int stSize;
    int fmt;
    int fixedFields;
    int result;

	stSize = atCache[atIx + 2];
	if (((((unsigned ) index)) >= 1) && ((((unsigned ) index)) <= (((unsigned ) stSize)))) {
		fmt = atCache[atIx + 3];
		if (fmt <= 4) {
			fixedFields = atCache[atIx + 4];
			return longAt(((((char *) rcvr)) + 4) + (((index + fixedFields) - 1) << 2));
		}
		if (fmt < 8) {
			result = longAt(((((char *) rcvr)) + 4) + ((index - 1) << 2));
			result = positive32BitIntegerFor(result);
			return result;
		}
		if (fmt >= 16) {
			return longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (24 << 2))))) + 4) + ((byteAt(((((char *) rcvr)) + 4) + (index - 1))) << 2));
		} else {
			return (((byteAt(((((char *) rcvr)) + 4) + (index - 1))) << 1) | 1);
		}
	}
	successFlag = 0;
}

int commonVariableatputcacheIndex(int rcvr, int index, int value, int atIx) {
    int stSize;
    int valToPut;
    int fmt;
    int fixedFields;

	stSize = atCache[atIx + 2];
	if (((((unsigned ) index)) >= 1) && ((((unsigned ) index)) <= (((unsigned ) stSize)))) {
		fmt = atCache[atIx + 3];
		if (fmt <= 4) {
			fixedFields = atCache[atIx + 4];
			/* begin storePointer:ofObject:withValue: */
			if (rcvr < youngStart) {
				possibleRootStoreIntovalue(rcvr, value);
			}
			return longAtput(((((char *) rcvr)) + 4) + (((index + fixedFields) - 1) << 2), value);
		}
		if (fmt < 8) {
			valToPut = positive32BitValueOf(value);
			if (successFlag) {
				longAtput(((((char *) rcvr)) + 4) + ((index - 1) << 2), valToPut);
			}
			return null;
		}
		if (fmt >= 16) {
			valToPut = asciiOfCharacter(value);
			if (!(successFlag)) {
				return null;
			}
		} else {
			valToPut = value;
		}
		if ((valToPut & 1)) {
			return byteAtput(((((char *) rcvr)) + 4) + (index - 1), (valToPut >> 1));
		}
	}
	successFlag = 0;
}

int compare31or32Bitsequal(int obj1, int obj2) {
	if (((obj1 & 1)) && ((obj2 & 1))) {
		return obj1 == obj2;
	}
	return (positive32BitValueOf(obj1)) == (positive32BitValueOf(obj2));
}

int containOnlyOopsand(int array1, int array2) {
    int fieldOffset;
    int methodHeader;
    int sz;
    int fmt;
    int header;
    int header1;
    int type;

	/* begin lastPointerOf: */
	header = longAt(array1);
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			fieldOffset = (6 + (fetchStackPointerOf(array1))) * 4;
			goto l1;
		}
		/* begin sizeBitsOfSafe: */
		header1 = longAt(array1);
		/* begin rightType: */
		if ((header1 & 252) == 0) {
			type = 0;
			goto l2;
		} else {
			if ((header1 & 126976) == 0) {
				type = 1;
				goto l2;
			} else {
				type = 3;
				goto l2;
			}
		}
	l2:	/* end rightType: */;
		if (type == 0) {
			sz = (longAt(array1 - 8)) & 4294967292U;
			goto l3;
		} else {
			sz = header1 & 252;
			goto l3;
		}
	l3:	/* end sizeBitsOfSafe: */;
		fieldOffset = sz - 4;
		goto l1;
	}
	if (fmt < 12) {
		fieldOffset = 0;
		goto l1;
	}
	methodHeader = longAt(array1 + 4);
	fieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
l1:	/* end lastPointerOf: */;
	while (fieldOffset >= 4) {
		if (((longAt(array1 + fieldOffset)) & 1)) {
			return 0;
		}
		if (((longAt(array2 + fieldOffset)) & 1)) {
			return 0;
		}
		fieldOffset -= 4;
	}
	return 1;
}

int copyBits(void) {
    int done;
    int dWid;
    int sxLowBits;
    int dxLowBits;
    int pixPerM1;
    int t;
    int integerValue;
    int integerPointer;
    int sp;

	clipRange();
	if ((bbW <= 0) || (bbH <= 0)) {
		affectedL = affectedR = affectedT = affectedB = 0;
		return null;
	}
	/* begin tryCopyingBitsQuickly */
	if (noSource) {
		done = 0;
		goto l2;
	}
	if (!(combinationRule == 34)) {
		done = 0;
		goto l2;
	}
	if (!(sourcePixSize == 32)) {
		done = 0;
		goto l2;
	}
	if (sourceForm == destForm) {
		done = 0;
		goto l2;
	}
	if (destPixSize < 8) {
		done = 0;
		goto l2;
	}
	if ((destPixSize == 8) && (colorMap == nilObj)) {
		done = 0;
		goto l2;
	}
	if (destPixSize == 32) {
		alphaSourceBlendBits32();
	}
	if (destPixSize == 16) {
		alphaSourceBlendBits16();
	}
	if (destPixSize == 8) {
		alphaSourceBlendBits8();
	}
	affectedL = dx;
	affectedR = dx + bbW;
	affectedT = dy;
	affectedB = dy + bbH;
	done = 1;
l2:	/* end tryCopyingBitsQuickly */;
	if (done) {
		return null;
	}
	destMaskAndPointerInit();
	bitCount = 0;
	if ((combinationRule == 30) || (combinationRule == 31)) {
		if (argumentCount == 1) {
			/* begin stackIntegerValue: */
			integerPointer = longAt(stackPointer - (0 * 4));
			if ((integerPointer & 1)) {
				sourceAlpha = (integerPointer >> 1);
				goto l1;
			} else {
				successFlag = 0;
				sourceAlpha = 0;
				goto l1;
			}
		l1:	/* end stackIntegerValue: */;
			if ((!(!successFlag)) && ((sourceAlpha >= 0) && (sourceAlpha <= 255))) {
				/* begin pop: */
				stackPointer -= 1 * 4;
			} else {
				return successFlag = 0;
			}
		} else {
			return successFlag = 0;
		}
	}
	if (noSource) {
		copyLoopNoSource();
	} else {
		/* begin checkSourceOverlap */
		if ((sourceForm == destForm) && (dy >= sy)) {
			if (dy > sy) {
				vDir = -1;
				sy = (sy + bbH) - 1;
				dy = (dy + bbH) - 1;
			} else {
				if ((dy == sy) && (dx > sx)) {
					hDir = -1;
					sx = (sx + bbW) - 1;
					dx = (dx + bbW) - 1;
					if (nWords > 1) {
						t = mask1;
						mask1 = mask2;
						mask2 = t;
					}
				}
			}
			destIndex = (destBits + 4) + (((dy * destRaster) + (dx / pixPerWord)) * 4);
			destDelta = 4 * ((destRaster * vDir) - (nWords * hDir));
		}
		if ((sourcePixSize != destPixSize) || (colorMap != nilObj)) {
			copyLoopPixMap();
		} else {
			/* begin sourceSkewAndPointerInit */
			pixPerM1 = pixPerWord - 1;
			sxLowBits = sx & pixPerM1;
			dxLowBits = dx & pixPerM1;
			if (hDir > 0) {
				dWid = ((bbW < (pixPerWord - dxLowBits)) ? bbW : (pixPerWord - dxLowBits));
				preload = (sxLowBits + dWid) > pixPerM1;
			} else {
				dWid = ((bbW < (dxLowBits + 1)) ? bbW : (dxLowBits + 1));
				preload = ((sxLowBits - dWid) + 1) < 0;
			}
			skew = (sxLowBits - dxLowBits) * destPixSize;
			if (preload) {
				if (skew < 0) {
					skew += 32;
				} else {
					skew -= 32;
				}
			}
			sourceIndex = (sourceBits + 4) + (((sy * sourceRaster) + (sx / (32 / sourcePixSize))) * 4);
			sourceDelta = 4 * ((sourceRaster * vDir) - (nWords * hDir));
			if (preload) {
				sourceDelta -= 4 * hDir;
			}
			copyLoop();
		}
	}
	if ((combinationRule == 22) || (combinationRule == 32)) {
		affectedL = affectedR = affectedT = affectedB = 0;
		/* begin pop: */
		stackPointer -= 1 * 4;
		/* begin pushInteger: */
		integerValue = bitCount;
		/* begin push: */
		longAtput(sp = stackPointer + 4, ((integerValue << 1) | 1));
		stackPointer = sp;
		return null;
	}
	if (hDir > 0) {
		affectedL = dx;
		affectedR = dx + bbW;
	} else {
		affectedL = (dx - bbW) + 1;
		affectedR = dx + 1;
	}
	if (vDir > 0) {
		affectedT = dy;
		affectedB = dy + bbH;
	} else {
		affectedT = (dy - bbH) + 1;
		affectedB = dy + 1;
	}
}

void copyBitsFromtoat(int startX, int stopX, int yValue) {
	destX = startX;
	destY = yValue;
	sourceX = startX;
	width = stopX - startX;
	copyBits();
}

void copyLoop(void) {
    int y;
    int prevWord;
    int skewWord;
    int mergeWord;
    int skewMask;
    int hInc;
    int (*mergeFnwith)(int, int);
    int i;
    int thisWord;
    int word;
    int halftoneWord;
    int notSkewMask;
    int unskew;

	mergeFnwith = ((int (*)(int, int)) (opTable[combinationRule + 1]));
	mergeFnwith;
	hInc = hDir * 4;
	if (skew == -32) {
		skew = unskew = skewMask = 0;
	} else {
		if (skew < 0) {
			unskew = skew + 32;
			skewMask = 4294967295U << (0 - skew);
		} else {
			if (skew == 0) {
				unskew = 0;
				skewMask = 4294967295U;
			} else {
				unskew = skew - 32;
				skewMask = ((unsigned) 4294967295U) >> skew;
			}
		}
	}
	notSkewMask = ~skewMask;
	if (noHalftone) {
		halftoneWord = 4294967295U;
		halftoneHeight = 0;
	} else {
		halftoneWord = longAt(halftoneBase);
	}
	y = dy;
	for (i = 1; i <= bbH; i += 1) {
		if (halftoneHeight > 1) {
			halftoneWord = longAt(halftoneBase + ((y % halftoneHeight) * 4));
			y += vDir;
		}
		if (preload) {
			prevWord = longAt(sourceIndex);
			sourceIndex += hInc;
		} else {
			prevWord = 0;
		}
		destMask = mask1;
		thisWord = longAt(sourceIndex);
		sourceIndex += hInc;
		skewWord = (((unskew < 0) ? ((unsigned) (prevWord & notSkewMask) >> -unskew) : ((unsigned) (prevWord & notSkewMask) << unskew))) | (((skew < 0) ? ((unsigned) (thisWord & skewMask) >> -skew) : ((unsigned) (thisWord & skewMask) << skew)));
		prevWord = thisWord;
		mergeWord = mergeFnwith(skewWord & halftoneWord, longAt(destIndex));
		longAtput(destIndex, (destMask & mergeWord) | ((~destMask) & (longAt(destIndex))));
		destIndex += hInc;
		destMask = 4294967295U;
		if (combinationRule == 3) {
			if (noHalftone && (notSkewMask == 0)) {
				for (word = 2; word <= (nWords - 1); word += 1) {
					thisWord = longAt(sourceIndex);
					sourceIndex += hInc;
					longAtput(destIndex, thisWord);
					destIndex += hInc;
				}
			} else {
				for (word = 2; word <= (nWords - 1); word += 1) {
					thisWord = longAt(sourceIndex);
					sourceIndex += hInc;
					skewWord = (((unskew < 0) ? ((unsigned) (prevWord & notSkewMask) >> -unskew) : ((unsigned) (prevWord & notSkewMask) << unskew))) | (((skew < 0) ? ((unsigned) (thisWord & skewMask) >> -skew) : ((unsigned) (thisWord & skewMask) << skew)));
					prevWord = thisWord;
					longAtput(destIndex, skewWord & halftoneWord);
					destIndex += hInc;
				}
			}
		} else {
			for (word = 2; word <= (nWords - 1); word += 1) {
				thisWord = longAt(sourceIndex);
				sourceIndex += hInc;
				skewWord = (((unskew < 0) ? ((unsigned) (prevWord & notSkewMask) >> -unskew) : ((unsigned) (prevWord & notSkewMask) << unskew))) | (((skew < 0) ? ((unsigned) (thisWord & skewMask) >> -skew) : ((unsigned) (thisWord & skewMask) << skew)));
				prevWord = thisWord;
				mergeWord = mergeFnwith(skewWord & halftoneWord, longAt(destIndex));
				longAtput(destIndex, mergeWord);
				destIndex += hInc;
			}
		}
		if (nWords > 1) {
			destMask = mask2;
			thisWord = longAt(sourceIndex);
			sourceIndex += hInc;
			skewWord = (((unskew < 0) ? ((unsigned) (prevWord & notSkewMask) >> -unskew) : ((unsigned) (prevWord & notSkewMask) << unskew))) | (((skew < 0) ? ((unsigned) (thisWord & skewMask) >> -skew) : ((unsigned) (thisWord & skewMask) << skew)));
			mergeWord = mergeFnwith(skewWord & halftoneWord, longAt(destIndex));
			longAtput(destIndex, (destMask & mergeWord) | ((~destMask) & (longAt(destIndex))));
			destIndex += hInc;
		}
		sourceIndex += sourceDelta;
		destIndex += destDelta;
	}
}

void copyLoopNoSource(void) {
    int mergeWord;
    int (*mergeFnwith)(int, int);
    int i;
    int word;
    int halftoneWord;

	mergeFnwith = ((int (*)(int, int)) (opTable[combinationRule + 1]));
	mergeFnwith;
	for (i = 1; i <= bbH; i += 1) {
		if (noHalftone) {
			halftoneWord = 4294967295U;
		} else {
			halftoneWord = longAt(halftoneBase + ((((dy + i) - 1) % halftoneHeight) * 4));
		}
		destMask = mask1;
		mergeWord = mergeFnwith(halftoneWord, longAt(destIndex));
		longAtput(destIndex, (destMask & mergeWord) | ((~destMask) & (longAt(destIndex))));
		destIndex += 4;
		destMask = 4294967295U;
		if (combinationRule == 3) {
			for (word = 2; word <= (nWords - 1); word += 1) {
				longAtput(destIndex, halftoneWord);
				destIndex += 4;
			}
		} else {
			for (word = 2; word <= (nWords - 1); word += 1) {
				mergeWord = mergeFnwith(halftoneWord, longAt(destIndex));
				longAtput(destIndex, mergeWord);
				destIndex += 4;
			}
		}
		if (nWords > 1) {
			destMask = mask2;
			mergeWord = mergeFnwith(halftoneWord, longAt(destIndex));
			longAtput(destIndex, (destMask & mergeWord) | ((~destMask) & (longAt(destIndex))));
			destIndex += 4;
		}
		destIndex += destDelta;
	}
}

void copyLoopPixMap(void) {
    int skewWord;
    int mergeWord;
    int srcPixPerWord;
    int scrStartBits;
    int nSourceIncs;
    int startBits;
    int sourcePixMask;
    int destPixMask;
    int endBits;
    int nullMap;
    int halftoneWord;
    int (*mergeFnwith)(int, int);
    int i;
    int word;
    int nPix;
    int nPix1;

	mergeFnwith = ((int (*)(int, int)) (opTable[combinationRule + 1]));
	mergeFnwith;
	srcPixPerWord = 32 / sourcePixSize;
	sourcePixMask = maskTable[sourcePixSize];
	destPixMask = maskTable[destPixSize];
	nullMap = colorMap == nilObj;
	sourceIndex = (sourceBits + 4) + (((sy * sourceRaster) + (sx / srcPixPerWord)) * 4);
	scrStartBits = srcPixPerWord - (sx & (srcPixPerWord - 1));
	if (bbW < scrStartBits) {
		nSourceIncs = 0;
	} else {
		nSourceIncs = ((bbW - scrStartBits) / srcPixPerWord) + 1;
	}
	sourceDelta = (sourceRaster - nSourceIncs) * 4;
	startBits = pixPerWord - (dx & (pixPerWord - 1));
	endBits = (((dx + bbW) - 1) & (pixPerWord - 1)) + 1;
	for (i = 1; i <= bbH; i += 1) {
		if (noHalftone) {
			halftoneWord = 4294967295U;
		} else {
			halftoneWord = longAt(halftoneBase + ((((dy + i) - 1) % halftoneHeight) * 4));
		}
		srcBitIndex = (sx & (srcPixPerWord - 1)) * sourcePixSize;
		destMask = mask1;
		if (bbW < startBits) {
			/* begin pickSourcePixels:nullMap:srcMask:destMask: */
			nPix = bbW;
			if (sourcePixSize >= 16) {
				skewWord = pickSourcePixelsRGBnullMapsrcMaskdestMask(nPix, nullMap, sourcePixMask, destPixMask);
				goto l1;
			}
			if (nullMap) {
				skewWord = pickSourcePixelsNullMapsrcMaskdestMask(nPix, sourcePixMask, destPixMask);
				goto l1;
			}
			skewWord = pickSourcePixelssrcMaskdestMask(nPix, sourcePixMask, destPixMask);
		l1:	/* end pickSourcePixels:nullMap:srcMask:destMask: */;
			skewWord = ((((startBits - bbW) * destPixSize) < 0) ? ((unsigned) skewWord >> -((startBits - bbW) * destPixSize)) : ((unsigned) skewWord << ((startBits - bbW) * destPixSize)));
		} else {
			/* begin pickSourcePixels:nullMap:srcMask:destMask: */
			if (sourcePixSize >= 16) {
				skewWord = pickSourcePixelsRGBnullMapsrcMaskdestMask(startBits, nullMap, sourcePixMask, destPixMask);
				goto l2;
			}
			if (nullMap) {
				skewWord = pickSourcePixelsNullMapsrcMaskdestMask(startBits, sourcePixMask, destPixMask);
				goto l2;
			}
			skewWord = pickSourcePixelssrcMaskdestMask(startBits, sourcePixMask, destPixMask);
		l2:	/* end pickSourcePixels:nullMap:srcMask:destMask: */;
		}
		for (word = 1; word <= nWords; word += 1) {
			mergeWord = mergeFnwith(skewWord & halftoneWord, (longAt(destIndex)) & destMask);
			longAtput(destIndex, (destMask & mergeWord) | ((~destMask) & (longAt(destIndex))));
			destIndex += 4;
			if (word >= (nWords - 1)) {
				if (!(word == nWords)) {
					destMask = mask2;
					/* begin pickSourcePixels:nullMap:srcMask:destMask: */
					if (sourcePixSize >= 16) {
						skewWord = pickSourcePixelsRGBnullMapsrcMaskdestMask(endBits, nullMap, sourcePixMask, destPixMask);
						goto l3;
					}
					if (nullMap) {
						skewWord = pickSourcePixelsNullMapsrcMaskdestMask(endBits, sourcePixMask, destPixMask);
						goto l3;
					}
					skewWord = pickSourcePixelssrcMaskdestMask(endBits, sourcePixMask, destPixMask);
				l3:	/* end pickSourcePixels:nullMap:srcMask:destMask: */;
					skewWord = ((((pixPerWord - endBits) * destPixSize) < 0) ? ((unsigned) skewWord >> -((pixPerWord - endBits) * destPixSize)) : ((unsigned) skewWord << ((pixPerWord - endBits) * destPixSize)));
				}
			} else {
				destMask = 4294967295U;
				/* begin pickSourcePixels:nullMap:srcMask:destMask: */
				nPix1 = pixPerWord;
				if (sourcePixSize >= 16) {
					skewWord = pickSourcePixelsRGBnullMapsrcMaskdestMask(nPix1, nullMap, sourcePixMask, destPixMask);
					goto l4;
				}
				if (nullMap) {
					skewWord = pickSourcePixelsNullMapsrcMaskdestMask(nPix1, sourcePixMask, destPixMask);
					goto l4;
				}
				skewWord = pickSourcePixelssrcMaskdestMask(nPix1, sourcePixMask, destPixMask);
			l4:	/* end pickSourcePixels:nullMap:srcMask:destMask: */;
			}
		}
		sourceIndex += sourceDelta;
		destIndex += destDelta;
	}
}

void createActualMessage(void) {
    int argumentArray;
    int message;
    int oop;
    int valuePointer;
    int lastIn;
    int in;
    int out;
    int sp;

	argumentArray = instantiateClassindexableSize(longAt(((((char *) specialObjectsOop)) + 4) + (7 << 2)), argumentCount);
	/* begin pushRemappableOop: */
	remapBuffer[remapBufferCount += 1] = argumentArray;
	message = instantiateClassindexableSize(longAt(((((char *) specialObjectsOop)) + 4) + (15 << 2)), 0);
	/* begin popRemappableOop */
	oop = remapBuffer[remapBufferCount];
	remapBufferCount -= 1;
	argumentArray = oop;
	if (argumentArray < youngStart) {
		beRootIfOld(argumentArray);
	}
	/* begin storePointer:ofObject:withValue: */
	valuePointer = messageSelector;
	if (message < youngStart) {
		possibleRootStoreIntovalue(message, valuePointer);
	}
	longAtput(((((char *) message)) + 4) + (0 << 2), valuePointer);
	/* begin storePointer:ofObject:withValue: */
	if (message < youngStart) {
		possibleRootStoreIntovalue(message, argumentArray);
	}
	longAtput(((((char *) message)) + 4) + (1 << 2), argumentArray);
	/* begin transfer:from:to: */
	in = (stackPointer - ((argumentCount - 1) * 4)) - 4;
	lastIn = in + (argumentCount * 4);
	out = (argumentArray + 4) - 4;
	while (in < lastIn) {
		longAtput(out += 4, longAt(in += 4));
	}
	/* begin pop: */
	stackPointer -= argumentCount * 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, message);
	stackPointer = sp;
	argumentCount = 1;
}

unsigned int * default8To32Table(void) {
    static unsigned int theTable[256] = { 
0x0, 0xFF000001, 0xFFFFFFFF, 0xFF808080, 0xFFFF0000, 0xFF00FF00, 0xFF0000FF, 0xFF00FFFF, 
0xFFFFFF00, 0xFFFF00FF, 0xFF202020, 0xFF404040, 0xFF606060, 0xFF9F9F9F, 0xFFBFBFBF, 0xFFDFDFDF, 
0xFF080808, 0xFF101010, 0xFF181818, 0xFF282828, 0xFF303030, 0xFF383838, 0xFF484848, 0xFF505050, 
0xFF585858, 0xFF686868, 0xFF707070, 0xFF787878, 0xFF878787, 0xFF8F8F8F, 0xFF979797, 0xFFA7A7A7, 
0xFFAFAFAF, 0xFFB7B7B7, 0xFFC7C7C7, 0xFFCFCFCF, 0xFFD7D7D7, 0xFFE7E7E7, 0xFFEFEFEF, 0xFFF7F7F7, 
0xFF000001, 0xFF003300, 0xFF006600, 0xFF009900, 0xFF00CC00, 0xFF00FF00, 0xFF000033, 0xFF003333, 
0xFF006633, 0xFF009933, 0xFF00CC33, 0xFF00FF33, 0xFF000066, 0xFF003366, 0xFF006666, 0xFF009966, 
0xFF00CC66, 0xFF00FF66, 0xFF000099, 0xFF003399, 0xFF006699, 0xFF009999, 0xFF00CC99, 0xFF00FF99, 
0xFF0000CC, 0xFF0033CC, 0xFF0066CC, 0xFF0099CC, 0xFF00CCCC, 0xFF00FFCC, 0xFF0000FF, 0xFF0033FF, 
0xFF0066FF, 0xFF0099FF, 0xFF00CCFF, 0xFF00FFFF, 0xFF330000, 0xFF333300, 0xFF336600, 0xFF339900, 
0xFF33CC00, 0xFF33FF00, 0xFF330033, 0xFF333333, 0xFF336633, 0xFF339933, 0xFF33CC33, 0xFF33FF33, 
0xFF330066, 0xFF333366, 0xFF336666, 0xFF339966, 0xFF33CC66, 0xFF33FF66, 0xFF330099, 0xFF333399, 
0xFF336699, 0xFF339999, 0xFF33CC99, 0xFF33FF99, 0xFF3300CC, 0xFF3333CC, 0xFF3366CC, 0xFF3399CC, 
0xFF33CCCC, 0xFF33FFCC, 0xFF3300FF, 0xFF3333FF, 0xFF3366FF, 0xFF3399FF, 0xFF33CCFF, 0xFF33FFFF, 
0xFF660000, 0xFF663300, 0xFF666600, 0xFF669900, 0xFF66CC00, 0xFF66FF00, 0xFF660033, 0xFF663333, 
0xFF666633, 0xFF669933, 0xFF66CC33, 0xFF66FF33, 0xFF660066, 0xFF663366, 0xFF666666, 0xFF669966, 
0xFF66CC66, 0xFF66FF66, 0xFF660099, 0xFF663399, 0xFF666699, 0xFF669999, 0xFF66CC99, 0xFF66FF99, 
0xFF6600CC, 0xFF6633CC, 0xFF6666CC, 0xFF6699CC, 0xFF66CCCC, 0xFF66FFCC, 0xFF6600FF, 0xFF6633FF, 
0xFF6666FF, 0xFF6699FF, 0xFF66CCFF, 0xFF66FFFF, 0xFF990000, 0xFF993300, 0xFF996600, 0xFF999900, 
0xFF99CC00, 0xFF99FF00, 0xFF990033, 0xFF993333, 0xFF996633, 0xFF999933, 0xFF99CC33, 0xFF99FF33, 
0xFF990066, 0xFF993366, 0xFF996666, 0xFF999966, 0xFF99CC66, 0xFF99FF66, 0xFF990099, 0xFF993399, 
0xFF996699, 0xFF999999, 0xFF99CC99, 0xFF99FF99, 0xFF9900CC, 0xFF9933CC, 0xFF9966CC, 0xFF9999CC, 
0xFF99CCCC, 0xFF99FFCC, 0xFF9900FF, 0xFF9933FF, 0xFF9966FF, 0xFF9999FF, 0xFF99CCFF, 0xFF99FFFF, 
0xFFCC0000, 0xFFCC3300, 0xFFCC6600, 0xFFCC9900, 0xFFCCCC00, 0xFFCCFF00, 0xFFCC0033, 0xFFCC3333, 
0xFFCC6633, 0xFFCC9933, 0xFFCCCC33, 0xFFCCFF33, 0xFFCC0066, 0xFFCC3366, 0xFFCC6666, 0xFFCC9966, 
0xFFCCCC66, 0xFFCCFF66, 0xFFCC0099, 0xFFCC3399, 0xFFCC6699, 0xFFCC9999, 0xFFCCCC99, 0xFFCCFF99, 
0xFFCC00CC, 0xFFCC33CC, 0xFFCC66CC, 0xFFCC99CC, 0xFFCCCCCC, 0xFFCCFFCC, 0xFFCC00FF, 0xFFCC33FF, 
0xFFCC66FF, 0xFFCC99FF, 0xFFCCCCFF, 0xFFCCFFFF, 0xFFFF0000, 0xFFFF3300, 0xFFFF6600, 0xFFFF9900, 
0xFFFFCC00, 0xFFFFFF00, 0xFFFF0033, 0xFFFF3333, 0xFFFF6633, 0xFFFF9933, 0xFFFFCC33, 0xFFFFFF33, 
0xFFFF0066, 0xFFFF3366, 0xFFFF6666, 0xFFFF9966, 0xFFFFCC66, 0xFFFFFF66, 0xFFFF0099, 0xFFFF3399, 
0xFFFF6699, 0xFFFF9999, 0xFFFFCC99, 0xFFFFFF99, 0xFFFF00CC, 0xFFFF33CC, 0xFFFF66CC, 0xFFFF99CC, 
0xFFFFCCCC, 0xFFFFFFCC, 0xFFFF00FF, 0xFFFF33FF, 0xFFFF66FF, 0xFFFF99FF, 0xFFFFCCFF, 0xFFFFFFFF};;

	return theTable;
}

int deltaFromtonSteps(int x1, int x2, int n) {
	if (x2 > x1) {
		return (((x2 - x1) + 16384) / (n + 1)) + 1;
	} else {
		if (x2 == x1) {
			return 0;
		}
		return 0 - ((((x1 - x2) + 16384) / (n + 1)) + 1);
	}
}

void destMaskAndPointerInit(void) {
    int startBits;
    int endBits;
    int pixPerM1;

	pixPerM1 = pixPerWord - 1;
	startBits = pixPerWord - (dx & pixPerM1);
	mask1 = ((unsigned) 4294967295U) >> (32 - (startBits * destPixSize));
	endBits = (((dx + bbW) - 1) & pixPerM1) + 1;
	mask2 = 4294967295U << (32 - (endBits * destPixSize));
	if (bbW < startBits) {
		mask1 = mask1 & mask2;
		mask2 = 0;
		nWords = 1;
	} else {
		nWords = (((bbW - startBits) + pixPerM1) / pixPerWord) + 1;
	}
	hDir = vDir = 1;
	destIndex = (destBits + 4) + (((dy * destRaster) + (dx / pixPerWord)) * 4);
	destDelta = 4 * ((destRaster * vDir) - (nWords * hDir));
}

int destinationWordwith(int sourceWord, int destinationWord) {
	return destinationWord;
}

int doPrimitiveDivby(int rcvr, int arg) {
    int posArg;
    int posRcvr;
    int result;
    int integerRcvr;
    int integerArg;

	if (((rcvr & arg) & 1) != 0) {
		integerRcvr = (rcvr >> 1);
		integerArg = (arg >> 1);
		successFlag = (integerArg != 0) && successFlag;
	} else {
		successFlag = 0;
	}
	if (!(successFlag)) {
		return 1;
	}
	if (integerRcvr > 0) {
		if (integerArg > 0) {
			result = integerRcvr / integerArg;
		} else {
			posArg = 0 - integerArg;
			result = 0 - ((integerRcvr + (posArg - 1)) / posArg);
		}
	} else {
		posRcvr = 0 - integerRcvr;
		if (integerArg > 0) {
			result = 0 - ((posRcvr + (integerArg - 1)) / integerArg);
		} else {
			posArg = 0 - integerArg;
			result = posRcvr / posArg;
		}
	}
	successFlag = ((result ^ (result << 1)) >= 0) && successFlag;
	return result;
}

int doPrimitiveModby(int rcvr, int arg) {
    int integerResult;
    int integerRcvr;
    int integerArg;

	if (((rcvr & arg) & 1) != 0) {
		integerRcvr = (rcvr >> 1);
		integerArg = (arg >> 1);
		successFlag = (integerArg != 0) && successFlag;
	} else {
		successFlag = 0;
	}
	if (!(successFlag)) {
		return 1;
	}
	integerResult = integerRcvr % integerArg;
	if (integerArg < 0) {
		if (integerResult > 0) {
			integerResult += integerArg;
		}
	} else {
		if (integerResult < 0) {
			integerResult += integerArg;
		}
	}
	successFlag = ((integerResult ^ (integerResult << 1)) >= 0) && successFlag;
	return integerResult;
}

void drawLoopXY(int xDelta, int yDelta) {
    int affL;
    int dx1;
    int dy1;
    int px;
    int py;
    int affR;
    int affT;
    int affB;
    int i;
    int P;

	if (xDelta > 0) {
		dx1 = 1;
	} else {
		if (xDelta == 0) {
			dx1 = 0;
		} else {
			dx1 = -1;
		}
	}
	if (yDelta > 0) {
		dy1 = 1;
	} else {
		if (yDelta == 0) {
			dy1 = 0;
		} else {
			dy1 = -1;
		}
	}
	px = yDelta * dy1;
	py = xDelta * dx1;
	affL = affT = 9999;
	affR = affB = -9999;
	if (py > px) {
		P = ((int) py >> 1);
		for (i = 1; i <= py; i += 1) {
			destX += dx1;
			if ((P -= px) < 0) {
				destY += dy1;
				P += py;
			}
			if (i < py) {
				copyBits();
				if ((affectedL < affectedR) && (affectedT < affectedB)) {
					affL = ((affL < affectedL) ? affL : affectedL);
					affR = ((affR < affectedR) ? affectedR : affR);
					affT = ((affT < affectedT) ? affT : affectedT);
					affB = ((affB < affectedB) ? affectedB : affB);
					if (((affR - affL) * (affB - affT)) > 4000) {
						affectedL = affL;
						affectedR = affR;
						affectedT = affT;
						affectedB = affB;
						showDisplayBits();
						affL = affT = 9999;
						affR = affB = -9999;
					}
				}
			}
		}
	} else {
		P = ((int) px >> 1);
		for (i = 1; i <= px; i += 1) {
			destY += dy1;
			if ((P -= py) < 0) {
				destX += dx1;
				P += px;
			}
			if (i < px) {
				copyBits();
				if ((affectedL < affectedR) && (affectedT < affectedB)) {
					affL = ((affL < affectedL) ? affL : affectedL);
					affR = ((affR < affectedR) ? affectedR : affR);
					affT = ((affT < affectedT) ? affT : affectedT);
					affB = ((affB < affectedB) ? affectedB : affB);
					if (((affR - affL) * (affB - affT)) > 4000) {
						affectedL = affL;
						affectedR = affR;
						affectedT = affT;
						affectedB = affB;
						showDisplayBits();
						affL = affT = 9999;
						affR = affB = -9999;
					}
				}
			}
		}
	}
	affectedL = affL;
	affectedR = affR;
	affectedT = affT;
	affectedB = affB;
	/* begin storeInteger:ofObject:withValue: */
	if ((destX ^ (destX << 1)) >= 0) {
		longAtput(((((char *) bitBltOop)) + 4) + (4 << 2), ((destX << 1) | 1));
	} else {
		successFlag = 0;
	}
	/* begin storeInteger:ofObject:withValue: */
	if ((destY ^ (destY << 1)) >= 0) {
		longAtput(((((char *) bitBltOop)) + 4) + (5 << 2), ((destY << 1) | 1));
	} else {
		successFlag = 0;
	}
}

void exchangeHashBitswith(int oop1, int oop2) {
    int hdr1;
    int hdr2;

	hdr1 = longAt(oop1);
	hdr2 = longAt(oop2);
	longAtput(oop1, (hdr1 & 3758227455U) | (hdr2 & 536739840));
	longAtput(oop2, (hdr2 & 3758227455U) | (hdr1 & 536739840));
}

int executeNewMethod(void) {
    int methodHeader;
    int i;
    int nilOop;
    int tempCount;
    int newContext;
    int initialIP;
    int cntxt;
    int tmp;

	if (primitiveIndex > 0) {
		primitiveResponse();
		if (successFlag) {
			return null;
		}
	}
	/* begin activateNewMethod */
	methodHeader = longAt(((((char *) newMethod)) + 4) + (0 << 2));
	/* begin allocateOrRecycleContext */
	if (freeContexts != 1) {
		cntxt = freeContexts;
		freeContexts = longAt(((((char *) cntxt)) + 4) + (0 << 2));
		newContext = cntxt;
		goto l1;
	}
	cntxt = instantiateContextsizeInBytes(longAt(((((char *) specialObjectsOop)) + 4) + (10 << 2)), 156);
	longAtput(((((char *) cntxt)) + 4) + (4 << 2), nilObj);
	newContext = cntxt;
l1:	/* end allocateOrRecycleContext */;
	initialIP = ((1 + ((((unsigned) methodHeader) >> 10) & 255)) * 4) + 1;
	tempCount = (((unsigned) methodHeader) >> 19) & 63;
	longAtput(((((char *) newContext)) + 4) + (0 << 2), activeContext);
	longAtput(((((char *) newContext)) + 4) + (1 << 2), ((initialIP << 1) | 1));
	longAtput(((((char *) newContext)) + 4) + (2 << 2), ((tempCount << 1) | 1));
	longAtput(((((char *) newContext)) + 4) + (3 << 2), newMethod);
	for (i = 0; i <= argumentCount; i += 1) {
		longAtput(((((char *) newContext)) + 4) + ((5 + i) << 2), longAt(stackPointer - ((argumentCount - i) * 4)));
	}
	nilOop = nilObj;
	for (i = (argumentCount + 1); i <= tempCount; i += 1) {
		longAtput(((((char *) newContext)) + 4) + ((5 + i) << 2), nilOop);
	}
	/* begin pop: */
	stackPointer -= (argumentCount + 1) * 4;
	reclaimableContextCount += 1;
	/* begin newActiveContext: */
	/* begin storeContextRegisters: */
	longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
	longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
	if (newContext < youngStart) {
		beRootIfOld(newContext);
	}
	activeContext = newContext;
	/* begin fetchContextRegisters: */
	tmp = longAt(((((char *) newContext)) + 4) + (3 << 2));
	if ((tmp & 1)) {
		tmp = longAt(((((char *) newContext)) + 4) + (5 << 2));
		if (tmp < youngStart) {
			beRootIfOld(tmp);
		}
	} else {
		tmp = newContext;
	}
	theHomeContext = tmp;
	receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
	method = longAt(((((char *) tmp)) + 4) + (3 << 2));
	tmp = ((longAt(((((char *) newContext)) + 4) + (1 << 2))) >> 1);
	instructionPointer = ((method + tmp) + 4) - 2;
	tmp = ((longAt(((((char *) newContext)) + 4) + (2 << 2))) >> 1);
	stackPointer = (newContext + 4) + (((6 + tmp) - 1) * 4);
	/* begin quickCheckForInterrupts */
	if ((interruptCheckCounter -= 1) <= 0) {
		checkForInterrupts();
	}
}

int extraHeaderBytes(int oopOrChunk) {
    int type;
    int extra;

	type = (longAt(oopOrChunk)) & 3;
	if (type > 1) {
		extra = 0;
	} else {
		if (type == 1) {
			extra = 4;
		} else {
			extra = 8;
		}
	}
	return extra;
}

int failed(void) {
	return !successFlag;
}

int fetchByteofObject(int byteIndex, int oop) {
	return byteAt(((((char *) oop)) + 4) + byteIndex);
}

int fetchClassOf(int oop) {
    int ccIndex;

	if ((oop & 1)) {
		return longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
	}
	ccIndex = (((unsigned) (longAt(oop))) >> 12) & 31;
	if (ccIndex == 0) {
		return (longAt(oop - 4)) & 4294967292U;
	} else {
		return longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
	}
}

int fetchClassOfNonInt(int oop) {
    int ccIndex;

	ccIndex = (((unsigned) (longAt(oop))) >> 12) & 31;
	if (ccIndex == 0) {
		return (longAt(oop - 4)) & 4294967292U;
	} else {
		return longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
	}
}

void fetchContextRegisters(int activeCntx) {
    int tmp;

	tmp = longAt(((((char *) activeCntx)) + 4) + (3 << 2));
	if ((tmp & 1)) {
		tmp = longAt(((((char *) activeCntx)) + 4) + (5 << 2));
		if (tmp < youngStart) {
			beRootIfOld(tmp);
		}
	} else {
		tmp = activeCntx;
	}
	theHomeContext = tmp;
	receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
	method = longAt(((((char *) tmp)) + 4) + (3 << 2));
	tmp = ((longAt(((((char *) activeCntx)) + 4) + (1 << 2))) >> 1);
	instructionPointer = ((method + tmp) + 4) - 2;
	tmp = ((longAt(((((char *) activeCntx)) + 4) + (2 << 2))) >> 1);
	stackPointer = (activeCntx + 4) + (((6 + tmp) - 1) * 4);
}

int fetchIntegerofObject(int fieldIndex, int objectPointer) {
    int intOop;

	intOop = longAt(((((char *) objectPointer)) + 4) + (fieldIndex << 2));
	if ((intOop & 1)) {
		return (intOop >> 1);
	} else {
		successFlag = 0;
		return 0;
	}
}

int fetchIntegerOrTruncFloatofObject(int fieldIndex, int objectPointer) {
    double trunc;
    double frac;
    double floatVal;
    int intOrFloat;

	intOrFloat = longAt(((((char *) objectPointer)) + 4) + (fieldIndex << 2));
	if ((intOrFloat & 1)) {
		return (intOrFloat >> 1);
	}
	if (successFlag) {
		fetchFloatAtinto(intOrFloat + 4, floatVal);
		frac = modf(floatVal, &trunc);
		success((-2147483648.0 <= trunc) && (trunc <= 2147483647.0));
	}
	if (successFlag) {
		return ((int) trunc);
	} else {
		return 0;
	}
}

int fetchPointerofObject(int fieldIndex, int oop) {
	return longAt(((((char *) oop)) + 4) + (fieldIndex << 2));
}

int fetchStackPointerOf(int aContext) {
    int sp;

	sp = longAt(((((char *) aContext)) + 4) + (2 << 2));
	if (!((sp & 1))) {
		return 0;
	}
	return (sp >> 1);
}

int fetchWordofObject(int fieldIndex, int oop) {
	return longAt(((((char *) oop)) + 4) + (fieldIndex << 2));
}

int fetchWordLengthOf(int objectPointer) {
    int sz;
    int header;

	/* begin sizeBitsOf: */
	header = longAt(objectPointer);
	if ((header & 3) == 0) {
		sz = (longAt(objectPointer - 8)) & 4294967292U;
		goto l1;
	} else {
		sz = header & 252;
		goto l1;
	}
l1:	/* end sizeBitsOf: */;
	return ((unsigned) (sz - 4)) >> 2;
}

int fileRecordSize(void) {
	return sizeof(SQFile);
}

SQFile * fileValueOf(int objectPointer) {
    int fileIndex;
    int successValue;

	/* begin success: */
	successValue = (((((unsigned) (longAt(objectPointer))) >> 8) & 15) >= 8) && ((lengthOf(objectPointer)) == (fileRecordSize()));
	successFlag = successValue && successFlag;
	if (successFlag) {
		fileIndex = objectPointer + 4;
		return (SQFile *) fileIndex;
	} else {
		return null;
	}
}

void finalizeReference(int oop) {
    int chunk;
    int oopGone;
    int firstField;
    int lastField;
    int i;
    int weakOop;
    int extra;
    int type;
    int extra1;
    int methodHeader;
    int sz;
    int fmt;
    int header;
    int header1;
    int type1;

	firstField = 4 + ((nonWeakFieldsOf(oop)) << 2);
	/* begin lastPointerOf: */
	header = longAt(oop);
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			lastField = (6 + (fetchStackPointerOf(oop))) * 4;
			goto l1;
		}
		/* begin sizeBitsOfSafe: */
		header1 = longAt(oop);
		/* begin rightType: */
		if ((header1 & 252) == 0) {
			type1 = 0;
			goto l2;
		} else {
			if ((header1 & 126976) == 0) {
				type1 = 1;
				goto l2;
			} else {
				type1 = 3;
				goto l2;
			}
		}
	l2:	/* end rightType: */;
		if (type1 == 0) {
			sz = (longAt(oop - 8)) & 4294967292U;
			goto l3;
		} else {
			sz = header1 & 252;
			goto l3;
		}
	l3:	/* end sizeBitsOfSafe: */;
		lastField = sz - 4;
		goto l1;
	}
	if (fmt < 12) {
		lastField = 0;
		goto l1;
	}
	methodHeader = longAt(oop + 4);
	lastField = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
l1:	/* end lastPointerOf: */;
	for (i = firstField; i <= lastField; i += 4) {
		weakOop = longAt(oop + i);
		if (!((weakOop == nilObj) || ((weakOop & 1)))) {
			if (weakOop < oop) {
				/* begin chunkFromOop: */
				/* begin extraHeaderBytes: */
				type = (longAt(weakOop)) & 3;
				if (type > 1) {
					extra1 = 0;
				} else {
					if (type == 1) {
						extra1 = 4;
					} else {
						extra1 = 8;
					}
				}
				extra = extra1;
				chunk = weakOop - extra;
				oopGone = ((longAt(chunk)) & 3) == 2;
			} else {
				oopGone = ((longAt(weakOop)) & 2147483648U) == 0;
			}
			if (oopGone) {
				longAtput(oop + i, nilObj);
				/* begin signalFinalization: */
				interruptCheckCounter = 0;
				pendingFinalizationSignals += 1;
			}
		}
	}
}

int findClassOfMethodforReceiver(int meth, int rcvr) {
    int methodArray;
    int done;
    int i;
    int classDict;
    int currClass;
    int classDictSize;
    int sz;
    int header;
    int ccIndex;
    int ccIndex1;

	/* begin fetchClassOf: */
	if ((rcvr & 1)) {
		currClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l2;
	}
	ccIndex = (((unsigned) (longAt(rcvr))) >> 12) & 31;
	if (ccIndex == 0) {
		currClass = (longAt(rcvr - 4)) & 4294967292U;
		goto l2;
	} else {
		currClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l2;
	}
l2:	/* end fetchClassOf: */;
	done = 0;
	while (!(done)) {
		classDict = longAt(((((char *) currClass)) + 4) + (1 << 2));
		/* begin fetchWordLengthOf: */
		/* begin sizeBitsOf: */
		header = longAt(classDict);
		if ((header & 3) == 0) {
			sz = (longAt(classDict - 8)) & 4294967292U;
			goto l1;
		} else {
			sz = header & 252;
			goto l1;
		}
	l1:	/* end sizeBitsOf: */;
		classDictSize = ((unsigned) (sz - 4)) >> 2;
		methodArray = longAt(((((char *) classDict)) + 4) + (1 << 2));
		i = 0;
		while (i < (classDictSize - 2)) {
			if (meth == (longAt(((((char *) methodArray)) + 4) + (i << 2)))) {
				return currClass;
			}
			i += 1;
		}
		currClass = longAt(((((char *) currClass)) + 4) + (0 << 2));
		done = currClass == nilObj;
	}
	/* begin fetchClassOf: */
	if ((rcvr & 1)) {
		return longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
	}
	ccIndex1 = (((unsigned) (longAt(rcvr))) >> 12) & 31;
	if (ccIndex1 == 0) {
		return (longAt(rcvr - 4)) & 4294967292U;
	} else {
		return longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex1 - 1) << 2));
	}
	return null;
}

void findNewMethodInClass(int cls) {
    int ok;
    int probe;
    int hash;

	/* begin lookupInMethodCacheSel:class: */
	hash = messageSelector ^ cls;
	probe = hash & 1020;
	if (((methodCache[probe + 1]) == messageSelector) && ((methodCache[probe + 2]) == cls)) {
		newMethod = methodCache[probe + 3];
		primitiveIndex = methodCache[probe + 4];
		ok = 1;
		goto l1;
	}
	probe = (((unsigned) hash) >> 1) & 1020;
	if (((methodCache[probe + 1]) == messageSelector) && ((methodCache[probe + 2]) == cls)) {
		newMethod = methodCache[probe + 3];
		primitiveIndex = methodCache[probe + 4];
		ok = 1;
		goto l1;
	}
	probe = (((unsigned) hash) >> 2) & 1020;
	if (((methodCache[probe + 1]) == messageSelector) && ((methodCache[probe + 2]) == cls)) {
		newMethod = methodCache[probe + 3];
		primitiveIndex = methodCache[probe + 4];
		ok = 1;
		goto l1;
	}
	ok = 0;
l1:	/* end lookupInMethodCacheSel:class: */;
	if (!(ok)) {
		lookupMethodInClass(cls);
		addToMethodCacheSelclassmethodprimIndex(messageSelector, cls, newMethod, primitiveIndex);
	}
}

int findSelectorOfMethodforReceiver(int meth, int rcvr) {
    int methodArray;
    int done;
    int i;
    int classDict;
    int currClass;
    int classDictSize;
    int sz;
    int header;
    int ccIndex;

	/* begin fetchClassOf: */
	if ((rcvr & 1)) {
		currClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l2;
	}
	ccIndex = (((unsigned) (longAt(rcvr))) >> 12) & 31;
	if (ccIndex == 0) {
		currClass = (longAt(rcvr - 4)) & 4294967292U;
		goto l2;
	} else {
		currClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l2;
	}
l2:	/* end fetchClassOf: */;
	done = 0;
	while (!(done)) {
		classDict = longAt(((((char *) currClass)) + 4) + (1 << 2));
		/* begin fetchWordLengthOf: */
		/* begin sizeBitsOf: */
		header = longAt(classDict);
		if ((header & 3) == 0) {
			sz = (longAt(classDict - 8)) & 4294967292U;
			goto l1;
		} else {
			sz = header & 252;
			goto l1;
		}
	l1:	/* end sizeBitsOf: */;
		classDictSize = ((unsigned) (sz - 4)) >> 2;
		methodArray = longAt(((((char *) classDict)) + 4) + (1 << 2));
		i = 0;
		while (i <= (classDictSize - 2)) {
			if (meth == (longAt(((((char *) methodArray)) + 4) + (i << 2)))) {
				return longAt(((((char *) classDict)) + 4) + ((i + 2) << 2));
			}
			i += 1;
		}
		currClass = longAt(((((char *) currClass)) + 4) + (0 << 2));
		done = currClass == nilObj;
	}
	return longAt(((((char *) specialObjectsOop)) + 4) + (20 << 2));
}

int firstAccessibleObject(void) {
    int obj;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header;
    int extra2;
    int type1;
    int extra11;

	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	obj = chunk + extra;
	while (obj < endOfMemory) {
		if (!(((longAt(obj)) & 3) == 2)) {
			return obj;
		}
		/* begin objectAfter: */
		;
		if (((longAt(obj)) & 3) == 2) {
			sz = (longAt(obj)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header = longAt(obj);
			if ((header & 3) == 0) {
				sz = (longAt(obj - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(obj + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		obj = (obj + sz) + extra2;
	}
	error("heap is empty");
}

int firstObject(void) {
    int chunk;
    int extra;
    int type;
    int extra1;

	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	return chunk + extra;
}

int fixedFieldsOfformatlength(int oop, int fmt, int wordLength) {
    int classFormat;
    int cls;
    int ccIndex;

	if ((fmt > 4) || (fmt == 2)) {
		return 0;
	}
	if (fmt < 2) {
		return wordLength;
	}
	/* begin fetchClassOf: */
	if ((oop & 1)) {
		cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l1;
	}
	ccIndex = (((unsigned) (longAt(oop))) >> 12) & 31;
	if (ccIndex == 0) {
		cls = (longAt(oop - 4)) & 4294967292U;
		goto l1;
	} else {
		cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l1;
	}
l1:	/* end fetchClassOf: */;
	classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
	return (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
}

int floatObjectOf(double aFloat) {
    int resultOop;

	resultOop = clone(longAt(((((char *) specialObjectsOop)) + 4) + (31 << 2)));
	storeFloatAtfrom(resultOop + 4, aFloat);
	return resultOop;
}

double floatValueOf(int oop) {
    double result;

	if (successFlag) {
		fetchFloatAtinto(oop + 4, result);
	} else {
		result = 0.0;
	}
	return result;
}

void flushMethodCache(void) {
    int i;

	for (i = 1; i <= 1024; i += 1) {
		methodCache[i] = 0;
	}
	for (i = 1; i <= 64; i += 1) {
		atCache[i] = 0;
	}
}

int formatOf(int oop) {
	return (((unsigned) (longAt(oop))) >> 8) & 15;
}

int formatOfClass(int classPointer) {
	return (longAt(((((char *) classPointer)) + 4) + (2 << 2))) - 1;
}

void fullCompaction(void) {
	compStart = lowestFreeAfter(startOfMemory());
	if (compStart == freeBlock) {
		initializeMemoryFirstFree(freeBlock);
	} else {
		while (compStart < freeBlock) {
			compStart = incCompBody();
		}
	}
}

void fullDisplayUpdate(void) {
    int displayObj;
    int dispBits;
    int dispBitsIndex;
    int h;
    int w;
    int d;

	displayObj = longAt(((((char *) specialObjectsOop)) + 4) + (14 << 2));
	if ((((((unsigned) (longAt(displayObj))) >> 8) & 15) <= 4) && ((lengthOf(displayObj)) >= 4)) {
		dispBits = longAt(((((char *) displayObj)) + 4) + (0 << 2));
		w = fetchIntegerofObject(1, displayObj);
		h = fetchIntegerofObject(2, displayObj);
		d = fetchIntegerofObject(3, displayObj);
		dispBitsIndex = dispBits + 4;
		ioShowDisplay(dispBitsIndex, w, h, d, 0, w, 0, h);
	}
}

void fullGC(void) {
    int startTime;
    int oop;
    int i;

	/* begin preGCAction */
	if (!(activeContext == nilObj)) {
		/* begin storeContextRegisters: */
		longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
		longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
	}
	startTime = ioMicroMSecs();
	/* begin clearRootsTable */
	for (i = 1; i <= rootTableCount; i += 1) {
		oop = rootTable[i];
		longAtput(oop, (longAt(oop)) & 3221225471U);
		rootTable[i] = 0;
	}
	rootTableCount = 0;
	youngStart = startOfMemory();
	markPhase();
	sweepPhase();
	/* begin fullCompaction */
	compStart = lowestFreeAfter(startOfMemory());
	if (compStart == freeBlock) {
		initializeMemoryFirstFree(freeBlock);
	} else {
		while (compStart < freeBlock) {
			compStart = incCompBody();
		}
	}
	allocationCount = 0;
	statFullGCs += 1;
	statFullGCMSecs += (ioMicroMSecs()) - startTime;
	youngStart = freeBlock;
	/* begin postGCAction */
	if (activeContext < youngStart) {
		beRootIfOld(activeContext);
	}
	if (theHomeContext < youngStart) {
		beRootIfOld(theHomeContext);
	}
}

int fwdBlockGet(int blkSize) {
	fwdTableNext += blkSize;
	if (fwdTableNext <= fwdTableLast) {
		return fwdTableNext;
	} else {
		return null;
	}
}

void fwdBlockValidate(int addr) {
	if (!((addr > endOfMemory) && ((addr <= fwdTableNext) && ((addr & 3) == 0)))) {
		error("invalid fwd table entry");
	}
}

int fwdTableInit(int blkSize) {
	/* begin setSizeOfFree:to: */
	longAtput(freeBlock, (4 & 4294967292U) | 2);
	endOfMemory = freeBlock + 4;
	/* begin setSizeOfFree:to: */
	longAtput(endOfMemory, (4 & 4294967292U) | 2);
	fwdTableNext = ((endOfMemory + 4) + 7) & 4294967288U;
	fwdTableLast = memoryLimit - blkSize;
	return (fwdTableLast - fwdTableNext) / blkSize;
}

int getLongFromFileswap(int f, int swapFlag) {
    int w;

	sqImageFileRead(&w, sizeof(char), 4, f);
	if (swapFlag) {
		return ((((((unsigned) w >> 24)) & 255) + ((((unsigned) w >> 8)) & 65280)) + ((((unsigned) w << 8)) & 16711680)) + ((((unsigned) w << 24)) & 4278190080U);
	} else {
		return w;
	}
}

int hashBitsOf(int oop) {
	return (((unsigned) (longAt(oop))) >> 17) & 4095;
}

int headerOf(int methodPointer) {
	return longAt(((((char *) methodPointer)) + 4) + (0 << 2));
}

int headerType(int oop) {
	return (longAt(oop)) & 3;
}

int ignoreSourceOrHalftone(int formPointer) {
	if (formPointer == nilObj) {
		return 1;
	}
	if (combinationRule == 0) {
		return 1;
	}
	if (combinationRule == 5) {
		return 1;
	}
	if (combinationRule == 10) {
		return 1;
	}
	if (combinationRule == 15) {
		return 1;
	}
	return 0;
}

int imageFormatVersion(void) {
	return 6502;
}

int incCompBody(void) {
    int bytesFreed;
    int newFreeChunk;
    int next;
    int bytesToMove;
    int w;
    int sz;
    int fwdBlock;
    int oop;
    int header;
    int firstWord;
    int lastWord;
    int newOop;
    int header1;
    int extra;
    int type;
    int extra1;
    int extra2;
    int type1;
    int extra11;
    int sz2;
    int fwdBlock1;
    int realHeader;
    int header2;
    int extra3;
    int type2;
    int extra12;
    int sz1;
    int header11;
    int extra21;
    int type11;
    int extra111;
    int fwdBlock2;
    int oop1;
    int bytesFreed1;
    int newOop1;
    int extra4;
    int type3;
    int extra13;
    int originalHeader;
    int originalHeaderType;
    int extra22;
    int type12;
    int extra112;
    int sz3;
    int fwdBlock3;
    int realHeader1;
    int header3;
    int extra5;
    int type4;
    int extra14;
    int sz11;
    int header12;
    int extra23;
    int type13;
    int extra113;

	fwdTableInit(8);
	/* begin incCompMakeFwd */
	bytesFreed1 = 0;
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type12 = (longAt(compStart)) & 3;
	if (type12 > 1) {
		extra112 = 0;
	} else {
		if (type12 == 1) {
			extra112 = 4;
		} else {
			extra112 = 8;
		}
	}
	extra22 = extra112;
	oop1 = compStart + extra22;
	while (oop1 < endOfMemory) {
		if (((longAt(oop1)) & 3) == 2) {
			bytesFreed1 += (longAt(oop1)) & 4294967292U;
		} else {
			/* begin fwdBlockGet: */
			fwdTableNext += 8;
			if (fwdTableNext <= fwdTableLast) {
				fwdBlock2 = fwdTableNext;
				goto l4;
			} else {
				fwdBlock2 = null;
				goto l4;
			}
		l4:	/* end fwdBlockGet: */;
			if (fwdBlock2 == null) {
				/* begin chunkFromOop: */
				/* begin extraHeaderBytes: */
				type3 = (longAt(oop1)) & 3;
				if (type3 > 1) {
					extra13 = 0;
				} else {
					if (type3 == 1) {
						extra13 = 4;
					} else {
						extra13 = 8;
					}
				}
				extra4 = extra13;
				compEnd = oop1 - extra4;
				bytesFreed = bytesFreed1;
				goto l5;
			}
			newOop1 = oop1 - bytesFreed1;
			/* begin initForwardBlock:mapping:to:withBackPtr: */
			originalHeader = longAt(oop1);
			;
			originalHeaderType = originalHeader & 3;
			longAtput(fwdBlock2, newOop1);
			longAtput(fwdBlock2 + 4, originalHeader);
			;
			longAtput(oop1, (((unsigned) fwdBlock2) >> 1) | (2147483648U | originalHeaderType));
		}
		/* begin objectAfterWhileForwarding: */
		header3 = longAt(oop1);
		if ((header3 & 2147483648U) == 0) {
			/* begin objectAfter: */
			;
			if (((longAt(oop1)) & 3) == 2) {
				sz11 = (longAt(oop1)) & 4294967292U;
			} else {
				/* begin sizeBitsOf: */
				header12 = longAt(oop1);
				if ((header12 & 3) == 0) {
					sz11 = (longAt(oop1 - 8)) & 4294967292U;
					goto l6;
				} else {
					sz11 = header12 & 252;
					goto l6;
				}
			l6:	/* end sizeBitsOf: */;
			}
			/* begin oopFromChunk: */
			/* begin extraHeaderBytes: */
			type13 = (longAt(oop1 + sz11)) & 3;
			if (type13 > 1) {
				extra113 = 0;
			} else {
				if (type13 == 1) {
					extra113 = 4;
				} else {
					extra113 = 8;
				}
			}
			extra23 = extra113;
			oop1 = (oop1 + sz11) + extra23;
			goto l7;
		}
		fwdBlock3 = (header3 & 2147483644) << 1;
		;
		realHeader1 = longAt(fwdBlock3 + 4);
		if ((realHeader1 & 3) == 0) {
			sz3 = (longAt(oop1 - 8)) & 4294967292U;
		} else {
			sz3 = realHeader1 & 252;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type4 = (longAt(oop1 + sz3)) & 3;
		if (type4 > 1) {
			extra14 = 0;
		} else {
			if (type4 == 1) {
				extra14 = 4;
			} else {
				extra14 = 8;
			}
		}
		extra5 = extra14;
		oop1 = (oop1 + sz3) + extra5;
	l7:	/* end objectAfterWhileForwarding: */;
	}
	compEnd = endOfMemory;
	bytesFreed = bytesFreed1;
l5:	/* end incCompMakeFwd */;
	mapPointersInObjectsFromto(youngStart, endOfMemory);
	/* begin incCompMove: */
	newOop = null;
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type1 = (longAt(compStart)) & 3;
	if (type1 > 1) {
		extra11 = 0;
	} else {
		if (type1 == 1) {
			extra11 = 4;
		} else {
			extra11 = 8;
		}
	}
	extra2 = extra11;
	oop = compStart + extra2;
	while (oop < compEnd) {
		/* begin objectAfterWhileForwarding: */
		header2 = longAt(oop);
		if ((header2 & 2147483648U) == 0) {
			/* begin objectAfter: */
			;
			if (((longAt(oop)) & 3) == 2) {
				sz1 = (longAt(oop)) & 4294967292U;
			} else {
				/* begin sizeBitsOf: */
				header11 = longAt(oop);
				if ((header11 & 3) == 0) {
					sz1 = (longAt(oop - 8)) & 4294967292U;
					goto l2;
				} else {
					sz1 = header11 & 252;
					goto l2;
				}
			l2:	/* end sizeBitsOf: */;
			}
			/* begin oopFromChunk: */
			/* begin extraHeaderBytes: */
			type11 = (longAt(oop + sz1)) & 3;
			if (type11 > 1) {
				extra111 = 0;
			} else {
				if (type11 == 1) {
					extra111 = 4;
				} else {
					extra111 = 8;
				}
			}
			extra21 = extra111;
			next = (oop + sz1) + extra21;
			goto l3;
		}
		fwdBlock1 = (header2 & 2147483644) << 1;
		;
		realHeader = longAt(fwdBlock1 + 4);
		if ((realHeader & 3) == 0) {
			sz2 = (longAt(oop - 8)) & 4294967292U;
		} else {
			sz2 = realHeader & 252;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type2 = (longAt(oop + sz2)) & 3;
		if (type2 > 1) {
			extra12 = 0;
		} else {
			if (type2 == 1) {
				extra12 = 4;
			} else {
				extra12 = 8;
			}
		}
		extra3 = extra12;
		next = (oop + sz2) + extra3;
	l3:	/* end objectAfterWhileForwarding: */;
		if (!(((longAt(oop)) & 3) == 2)) {
			fwdBlock = ((longAt(oop)) & 2147483644) << 1;
			;
			newOop = longAt(fwdBlock);
			header = longAt(fwdBlock + 4);
			longAtput(oop, header);
			bytesToMove = oop - newOop;
			/* begin sizeBitsOf: */
			header1 = longAt(oop);
			if ((header1 & 3) == 0) {
				sz = (longAt(oop - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header1 & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
			firstWord = oop - (extraHeaderBytes(oop));
			lastWord = (oop + sz) - 4;
			for (w = firstWord; w <= lastWord; w += 4) {
				longAtput(w - bytesToMove, longAt(w));
			}
		}
		oop = next;
	}
	if (newOop == null) {
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type = (longAt(compStart)) & 3;
		if (type > 1) {
			extra1 = 0;
		} else {
			if (type == 1) {
				extra1 = 4;
			} else {
				extra1 = 8;
			}
		}
		extra = extra1;
		oop = compStart + extra;
		if ((((longAt(oop)) & 3) == 2) && ((objectAfter(oop)) == (oopFromChunk(compEnd)))) {
			newFreeChunk = oop;
		} else {
			newFreeChunk = freeBlock;
		}
	} else {
		newFreeChunk = newOop + (sizeBitsOf(newOop));
		/* begin setSizeOfFree:to: */
		longAtput(newFreeChunk, (bytesFreed & 4294967292U) | 2);
	}
	;
	if ((objectAfter(newFreeChunk)) == endOfMemory) {
		initializeMemoryFirstFree(newFreeChunk);
	} else {
		initializeMemoryFirstFree(freeBlock);
	}
	return newFreeChunk;
}

int incCompMakeFwd(void) {
    int fwdBlock;
    int oop;
    int bytesFreed;
    int newOop;
    int extra;
    int type;
    int extra1;
    int originalHeader;
    int originalHeaderType;
    int extra2;
    int type1;
    int extra11;
    int sz;
    int fwdBlock1;
    int realHeader;
    int header;
    int extra3;
    int type2;
    int extra12;
    int sz1;
    int header1;
    int extra21;
    int type11;
    int extra111;

	bytesFreed = 0;
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type1 = (longAt(compStart)) & 3;
	if (type1 > 1) {
		extra11 = 0;
	} else {
		if (type1 == 1) {
			extra11 = 4;
		} else {
			extra11 = 8;
		}
	}
	extra2 = extra11;
	oop = compStart + extra2;
	while (oop < endOfMemory) {
		if (((longAt(oop)) & 3) == 2) {
			bytesFreed += (longAt(oop)) & 4294967292U;
		} else {
			/* begin fwdBlockGet: */
			fwdTableNext += 8;
			if (fwdTableNext <= fwdTableLast) {
				fwdBlock = fwdTableNext;
				goto l1;
			} else {
				fwdBlock = null;
				goto l1;
			}
		l1:	/* end fwdBlockGet: */;
			if (fwdBlock == null) {
				/* begin chunkFromOop: */
				/* begin extraHeaderBytes: */
				type = (longAt(oop)) & 3;
				if (type > 1) {
					extra1 = 0;
				} else {
					if (type == 1) {
						extra1 = 4;
					} else {
						extra1 = 8;
					}
				}
				extra = extra1;
				compEnd = oop - extra;
				return bytesFreed;
			}
			newOop = oop - bytesFreed;
			/* begin initForwardBlock:mapping:to:withBackPtr: */
			originalHeader = longAt(oop);
			;
			originalHeaderType = originalHeader & 3;
			longAtput(fwdBlock, newOop);
			longAtput(fwdBlock + 4, originalHeader);
			;
			longAtput(oop, (((unsigned) fwdBlock) >> 1) | (2147483648U | originalHeaderType));
		}
		/* begin objectAfterWhileForwarding: */
		header = longAt(oop);
		if ((header & 2147483648U) == 0) {
			/* begin objectAfter: */
			;
			if (((longAt(oop)) & 3) == 2) {
				sz1 = (longAt(oop)) & 4294967292U;
			} else {
				/* begin sizeBitsOf: */
				header1 = longAt(oop);
				if ((header1 & 3) == 0) {
					sz1 = (longAt(oop - 8)) & 4294967292U;
					goto l2;
				} else {
					sz1 = header1 & 252;
					goto l2;
				}
			l2:	/* end sizeBitsOf: */;
			}
			/* begin oopFromChunk: */
			/* begin extraHeaderBytes: */
			type11 = (longAt(oop + sz1)) & 3;
			if (type11 > 1) {
				extra111 = 0;
			} else {
				if (type11 == 1) {
					extra111 = 4;
				} else {
					extra111 = 8;
				}
			}
			extra21 = extra111;
			oop = (oop + sz1) + extra21;
			goto l3;
		}
		fwdBlock1 = (header & 2147483644) << 1;
		;
		realHeader = longAt(fwdBlock1 + 4);
		if ((realHeader & 3) == 0) {
			sz = (longAt(oop - 8)) & 4294967292U;
		} else {
			sz = realHeader & 252;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type2 = (longAt(oop + sz)) & 3;
		if (type2 > 1) {
			extra12 = 0;
		} else {
			if (type2 == 1) {
				extra12 = 4;
			} else {
				extra12 = 8;
			}
		}
		extra3 = extra12;
		oop = (oop + sz) + extra3;
	l3:	/* end objectAfterWhileForwarding: */;
	}
	compEnd = endOfMemory;
	return bytesFreed;
}

int incCompMove(int bytesFreed) {
    int newFreeChunk;
    int next;
    int bytesToMove;
    int w;
    int sz;
    int fwdBlock;
    int oop;
    int header;
    int firstWord;
    int lastWord;
    int newOop;
    int header1;
    int extra;
    int type;
    int extra1;
    int extra2;
    int type1;
    int extra11;
    int sz2;
    int fwdBlock1;
    int realHeader;
    int header2;
    int extra3;
    int type2;
    int extra12;
    int sz1;
    int header11;
    int extra21;
    int type11;
    int extra111;

	newOop = null;
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type1 = (longAt(compStart)) & 3;
	if (type1 > 1) {
		extra11 = 0;
	} else {
		if (type1 == 1) {
			extra11 = 4;
		} else {
			extra11 = 8;
		}
	}
	extra2 = extra11;
	oop = compStart + extra2;
	while (oop < compEnd) {
		/* begin objectAfterWhileForwarding: */
		header2 = longAt(oop);
		if ((header2 & 2147483648U) == 0) {
			/* begin objectAfter: */
			;
			if (((longAt(oop)) & 3) == 2) {
				sz1 = (longAt(oop)) & 4294967292U;
			} else {
				/* begin sizeBitsOf: */
				header11 = longAt(oop);
				if ((header11 & 3) == 0) {
					sz1 = (longAt(oop - 8)) & 4294967292U;
					goto l2;
				} else {
					sz1 = header11 & 252;
					goto l2;
				}
			l2:	/* end sizeBitsOf: */;
			}
			/* begin oopFromChunk: */
			/* begin extraHeaderBytes: */
			type11 = (longAt(oop + sz1)) & 3;
			if (type11 > 1) {
				extra111 = 0;
			} else {
				if (type11 == 1) {
					extra111 = 4;
				} else {
					extra111 = 8;
				}
			}
			extra21 = extra111;
			next = (oop + sz1) + extra21;
			goto l3;
		}
		fwdBlock1 = (header2 & 2147483644) << 1;
		;
		realHeader = longAt(fwdBlock1 + 4);
		if ((realHeader & 3) == 0) {
			sz2 = (longAt(oop - 8)) & 4294967292U;
		} else {
			sz2 = realHeader & 252;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type2 = (longAt(oop + sz2)) & 3;
		if (type2 > 1) {
			extra12 = 0;
		} else {
			if (type2 == 1) {
				extra12 = 4;
			} else {
				extra12 = 8;
			}
		}
		extra3 = extra12;
		next = (oop + sz2) + extra3;
	l3:	/* end objectAfterWhileForwarding: */;
		if (!(((longAt(oop)) & 3) == 2)) {
			fwdBlock = ((longAt(oop)) & 2147483644) << 1;
			;
			newOop = longAt(fwdBlock);
			header = longAt(fwdBlock + 4);
			longAtput(oop, header);
			bytesToMove = oop - newOop;
			/* begin sizeBitsOf: */
			header1 = longAt(oop);
			if ((header1 & 3) == 0) {
				sz = (longAt(oop - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header1 & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
			firstWord = oop - (extraHeaderBytes(oop));
			lastWord = (oop + sz) - 4;
			for (w = firstWord; w <= lastWord; w += 4) {
				longAtput(w - bytesToMove, longAt(w));
			}
		}
		oop = next;
	}
	if (newOop == null) {
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type = (longAt(compStart)) & 3;
		if (type > 1) {
			extra1 = 0;
		} else {
			if (type == 1) {
				extra1 = 4;
			} else {
				extra1 = 8;
			}
		}
		extra = extra1;
		oop = compStart + extra;
		if ((((longAt(oop)) & 3) == 2) && ((objectAfter(oop)) == (oopFromChunk(compEnd)))) {
			newFreeChunk = oop;
		} else {
			newFreeChunk = freeBlock;
		}
	} else {
		newFreeChunk = newOop + (sizeBitsOf(newOop));
		/* begin setSizeOfFree:to: */
		longAtput(newFreeChunk, (bytesFreed & 4294967292U) | 2);
	}
	;
	if ((objectAfter(newFreeChunk)) == endOfMemory) {
		initializeMemoryFirstFree(newFreeChunk);
	} else {
		initializeMemoryFirstFree(freeBlock);
	}
	return newFreeChunk;
}

void incrementalCompaction(void) {
	if (compStart == freeBlock) {
		initializeMemoryFirstFree(freeBlock);
	} else {
		incCompBody();
	}
}

void incrementalGC(void) {
    int startTime;
    int survivorCount;
    int oop;
    int i;

	if (rootTableCount >= 500) {
		statRootTableOverflows += 1;
		fullGC();
	} else {
		/* begin preGCAction */
		if (!(activeContext == nilObj)) {
			/* begin storeContextRegisters: */
			longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
			longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
		}
		startTime = ioMicroMSecs();
		markPhase();
		survivorCount = sweepPhase();
		/* begin incrementalCompaction */
		if (compStart == freeBlock) {
			initializeMemoryFirstFree(freeBlock);
		} else {
			incCompBody();
		}
		allocationCount = 0;
		statIncrGCs += 1;
		statIncrGCMSecs += (ioMicroMSecs()) - startTime;
		if (survivorCount > tenuringThreshold) {
			statTenures += 1;
			/* begin clearRootsTable */
			for (i = 1; i <= rootTableCount; i += 1) {
				oop = rootTable[i];
				longAtput(oop, (longAt(oop)) & 3221225471U);
				rootTable[i] = 0;
			}
			rootTableCount = 0;
			youngStart = freeBlock;
		}
		/* begin postGCAction */
		if (activeContext < youngStart) {
			beRootIfOld(activeContext);
		}
		if (theHomeContext < youngStart) {
			beRootIfOld(theHomeContext);
		}
	}
}

void initBBOpTable(void) {
	opTable[0+1] = (int)clearWordwith;
	opTable[1+1] = (int)bitAndwith;
	opTable[2+1] = (int)bitAndInvertwith;
	opTable[3+1] = (int)sourceWordwith;
	opTable[4+1] = (int)bitInvertAndwith;
	opTable[5+1] = (int)destinationWordwith;
	opTable[6+1] = (int)bitXorwith;
	opTable[7+1] = (int)bitOrwith;
	opTable[8+1] = (int)bitInvertAndInvertwith;
	opTable[9+1] = (int)bitInvertXorwith;
	opTable[10+1] = (int)bitInvertDestinationwith;
	opTable[11+1] = (int)bitOrInvertwith;
	opTable[12+1] = (int)bitInvertSourcewith;
	opTable[13+1] = (int)bitInvertOrwith;
	opTable[14+1] = (int)bitInvertOrInvertwith;
	opTable[15+1] = (int)destinationWordwith;
	opTable[16+1] = (int)destinationWordwith;
	opTable[17+1] = (int)destinationWordwith;
	opTable[18+1] = (int)addWordwith;
	opTable[19+1] = (int)subWordwith;
	opTable[20+1] = (int)rgbAddwith;
	opTable[21+1] = (int)rgbSubwith;
	opTable[22+1] = (int)OLDrgbDiffwith;
	opTable[23+1] = (int)OLDtallyIntoMapwith;
	opTable[24+1] = (int)alphaBlendwith;
	opTable[25+1] = (int)pixPaintwith;
	opTable[26+1] = (int)pixMaskwith;
	opTable[27+1] = (int)rgbMaxwith;
	opTable[28+1] = (int)rgbMinwith;
	opTable[29+1] = (int)rgbMinInvertwith;
	opTable[30+1] = (int)alphaBlendConstwith;
	opTable[31+1] = (int)alphaPaintConstwith;
	opTable[32+1] = (int)rgbDiffwith;
	opTable[33+1] = (int)tallyIntoMapwith;
	opTable[34+1] = (int)alphaBlendScaledwith;
}

void initForwardBlockmappingtowithBackPtr(int fwdBlock, int oop, int newOop, int backFlag) {
    int originalHeader;
    int originalHeaderType;

	originalHeader = longAt(oop);
	;
	originalHeaderType = originalHeader & 3;
	longAtput(fwdBlock, newOop);
	longAtput(fwdBlock + 4, originalHeader);
	if (backFlag) {
		longAtput(fwdBlock + 8, oop);
	}
	longAtput(oop, (((unsigned) fwdBlock) >> 1) | (2147483648U | originalHeaderType));
}

int initialInstanceOf(int classPointer) {
    int thisClass;
    int thisObj;
    int ccIndex;
    int obj;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header;
    int extra2;
    int type1;
    int extra11;

	/* begin firstAccessibleObject */
	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	obj = chunk + extra;
	while (obj < endOfMemory) {
		if (!(((longAt(obj)) & 3) == 2)) {
			thisObj = obj;
			goto l3;
		}
		/* begin objectAfter: */
		;
		if (((longAt(obj)) & 3) == 2) {
			sz = (longAt(obj)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header = longAt(obj);
			if ((header & 3) == 0) {
				sz = (longAt(obj - 8)) & 4294967292U;
				goto l2;
			} else {
				sz = header & 252;
				goto l2;
			}
		l2:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(obj + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		obj = (obj + sz) + extra2;
	}
	error("heap is empty");
l3:	/* end firstAccessibleObject */;
	while (!(thisObj == null)) {
		/* begin fetchClassOf: */
		if ((thisObj & 1)) {
			thisClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l1;
		}
		ccIndex = (((unsigned) (longAt(thisObj))) >> 12) & 31;
		if (ccIndex == 0) {
			thisClass = (longAt(thisObj - 4)) & 4294967292U;
			goto l1;
		} else {
			thisClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l1;
		}
	l1:	/* end fetchClassOf: */;
		if (thisClass == classPointer) {
			return thisObj;
		}
		thisObj = accessibleObjectAfter(thisObj);
	}
	return nilObj;
}

void initializeInterpreter(int bytesToShift) {
    int i;
    int sched;
    int proc;
    int activeCntx;
    int tmp;

	initializeObjectMemory(bytesToShift);
	initBBOpTable();
	activeContext = nilObj;
	theHomeContext = nilObj;
	method = nilObj;
	receiver = nilObj;
	messageSelector = nilObj;
	newMethod = nilObj;
	/* begin flushMethodCache */
	for (i = 1; i <= 1024; i += 1) {
		methodCache[i] = 0;
	}
	for (i = 1; i <= 64; i += 1) {
		atCache[i] = 0;
	}
	/* begin loadInitialContext */
	sched = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2));
	proc = longAt(((((char *) sched)) + 4) + (1 << 2));
	activeContext = longAt(((((char *) proc)) + 4) + (1 << 2));
	if (activeContext < youngStart) {
		beRootIfOld(activeContext);
	}
	/* begin fetchContextRegisters: */
	activeCntx = activeContext;
	tmp = longAt(((((char *) activeCntx)) + 4) + (3 << 2));
	if ((tmp & 1)) {
		tmp = longAt(((((char *) activeCntx)) + 4) + (5 << 2));
		if (tmp < youngStart) {
			beRootIfOld(tmp);
		}
	} else {
		tmp = activeCntx;
	}
	theHomeContext = tmp;
	receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
	method = longAt(((((char *) tmp)) + 4) + (3 << 2));
	tmp = ((longAt(((((char *) activeCntx)) + 4) + (1 << 2))) >> 1);
	instructionPointer = ((method + tmp) + 4) - 2;
	tmp = ((longAt(((((char *) activeCntx)) + 4) + (2 << 2))) >> 1);
	stackPointer = (activeCntx + 4) + (((6 + tmp) - 1) * 4);
	reclaimableContextCount = 0;
	interruptCheckCounter = 0;
	nextPollTick = 0;
	nextWakeupTick = 0;
	lastTick = 0;
	interruptKeycode = 2094;
	interruptPending = 0;
	semaphoresToSignalCount = 0;
	deferDisplayUpdates = 0;
	pendingFinalizationSignals = 0;
}

void initializeMemoryFirstFree(int firstFree) {
    int fwdBlockBytes;

	fwdBlockBytes = 10000;
	if (!((memoryLimit - fwdBlockBytes) >= (firstFree + 4))) {
		fwdBlockBytes = memoryLimit - (firstFree + 4);
	}
	endOfMemory = memoryLimit - fwdBlockBytes;
	freeBlock = firstFree;
	/* begin setSizeOfFree:to: */
	longAtput(freeBlock, ((endOfMemory - firstFree) & 4294967292U) | 2);
	/* begin setSizeOfFree:to: */
	longAtput(endOfMemory, (4 & 4294967292U) | 2);
	;
}

void initializeObjectMemory(int bytesToShift) {
    int oop;
    int header;
    int newClassOop;
    int fieldAddr;
    int fieldOop;
    int classHeader;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header1;
    int extra2;
    int type1;
    int extra11;

	youngStart = endOfMemory;
	initializeMemoryFirstFree(endOfMemory);
	/* begin adjustAllOopsBy: */
	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	oop = chunk + extra;
	while (oop < endOfMemory) {
		if (!(((longAt(oop)) & 3) == 2)) {
			header = longAt(oop);
			longAtput(oop, header & 3221225471U);
			/* begin adjustFieldsAndClassOf:by: */
			fieldAddr = oop + (lastPointerOf(oop));
			while (fieldAddr > oop) {
				fieldOop = longAt(fieldAddr);
				if (!((fieldOop & 1))) {
					longAtput(fieldAddr, fieldOop + bytesToShift);
				}
				fieldAddr -= 4;
			}
			if (((longAt(oop)) & 3) != 3) {
				classHeader = longAt(oop - 4);
				newClassOop = (classHeader & 4294967292U) + bytesToShift;
				longAtput(oop - 4, newClassOop | (classHeader & 3));
			}
		}
		/* begin objectAfter: */
		;
		if (((longAt(oop)) & 3) == 2) {
			sz = (longAt(oop)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header1 = longAt(oop);
			if ((header1 & 3) == 0) {
				sz = (longAt(oop - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header1 & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(oop + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		oop = (oop + sz) + extra2;
	}
	specialObjectsOop += bytesToShift;
	nilObj = longAt(((((char *) specialObjectsOop)) + 4) + (0 << 2));
	falseObj = longAt(((((char *) specialObjectsOop)) + 4) + (1 << 2));
	trueObj = longAt(((((char *) specialObjectsOop)) + 4) + (2 << 2));
	rootTableCount = 0;
	child = 0;
	field = 0;
	parentField = 0;
	freeContexts = 1;
	allocationCount = 0;
	lowSpaceThreshold = 0;
	signalLowSpace = 0;
	compStart = 0;
	compEnd = 0;
	fwdTableNext = 0;
	fwdTableLast = 0;
	remapBufferCount = 0;
	allocationsBetweenGCs = 4000;
	tenuringThreshold = 2000;
	statFullGCs = 0;
	statFullGCMSecs = 0;
	statIncrGCs = 0;
	statIncrGCMSecs = 0;
	statTenures = 0;
	statRootTableOverflows = 0;
	displayBits = 0;
}

int installinAtCacheatstring(int rcvr, int *cache, int atIx, int stringy) {
    int hdr;
    int totalLength;
    int fmt;
    int fixedFields;
    int sz;
    int classFormat;
    int cls;
    int ccIndex;

	hdr = longAt(rcvr);
	fmt = (((unsigned) hdr) >> 8) & 15;
	if ((fmt == 3) && (isContextHeader(hdr))) {
		return successFlag = 0;
	}
	/* begin lengthOf:baseHeader:format: */
	if ((hdr & 3) == 0) {
		sz = (longAt(rcvr - 8)) & 4294967292U;
	} else {
		sz = hdr & 252;
	}
	if (fmt < 8) {
		totalLength = ((unsigned) (sz - 4)) >> 2;
		goto l1;
	} else {
		totalLength = (sz - 4) - (fmt & 3);
		goto l1;
	}
l1:	/* end lengthOf:baseHeader:format: */;
	/* begin fixedFieldsOf:format:length: */
	if ((fmt > 4) || (fmt == 2)) {
		fixedFields = 0;
		goto l2;
	}
	if (fmt < 2) {
		fixedFields = totalLength;
		goto l2;
	}
	/* begin fetchClassOf: */
	if ((rcvr & 1)) {
		cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l3;
	}
	ccIndex = (((unsigned) (longAt(rcvr))) >> 12) & 31;
	if (ccIndex == 0) {
		cls = (longAt(rcvr - 4)) & 4294967292U;
		goto l3;
	} else {
		cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l3;
	}
l3:	/* end fetchClassOf: */;
	classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
	fixedFields = (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
l2:	/* end fixedFieldsOf:format:length: */;
	cache[atIx + 1] = rcvr;
	if (stringy) {
		cache[atIx + 3] = (fmt + 16);
	} else {
		cache[atIx + 3] = fmt;
	}
	cache[atIx + 4] = fixedFields;
	cache[atIx + 2] = (totalLength - fixedFields);
}

int instanceAfter(int objectPointer) {
    int thisClass;
    int classPointer;
    int thisObj;
    int ccIndex;
    int ccIndex1;

	/* begin fetchClassOf: */
	if ((objectPointer & 1)) {
		classPointer = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l2;
	}
	ccIndex1 = (((unsigned) (longAt(objectPointer))) >> 12) & 31;
	if (ccIndex1 == 0) {
		classPointer = (longAt(objectPointer - 4)) & 4294967292U;
		goto l2;
	} else {
		classPointer = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex1 - 1) << 2));
		goto l2;
	}
l2:	/* end fetchClassOf: */;
	thisObj = accessibleObjectAfter(objectPointer);
	while (!(thisObj == null)) {
		/* begin fetchClassOf: */
		if ((thisObj & 1)) {
			thisClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l1;
		}
		ccIndex = (((unsigned) (longAt(thisObj))) >> 12) & 31;
		if (ccIndex == 0) {
			thisClass = (longAt(thisObj - 4)) & 4294967292U;
			goto l1;
		} else {
			thisClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l1;
		}
	l1:	/* end fetchClassOf: */;
		if (thisClass == classPointer) {
			return thisObj;
		}
		thisObj = accessibleObjectAfter(thisObj);
	}
	return nilObj;
}

int instantiateClassindexableSize(int classPointer, int size) {
    int sizeHiBits;
    int newObj;
    int binc;
    int hash;
    int header1;
    int header2;
    int header3;
    int hdrSize;
    int byteSize;
    int format;
    int inc;
    int cClass;
    int fillWord;
    int i;
    int newObj1;
    int remappedClassOop;
    int end;
    int oop;
    int newFreeSize;
    int enoughSpace;
    int newChunk;
    int minFree;

	;
	/* begin newObjectHash */
	lastHash = (13849 + (27181 * lastHash)) & 65535;
	hash = lastHash;
	header1 = (longAt(((((char *) classPointer)) + 4) + (2 << 2))) - 1;
	sizeHiBits = ((unsigned) (header1 & 393216)) >> 9;
	header1 = (header1 & 131071) | ((hash << 17) & 536739840);
	header2 = classPointer;
	header3 = 0;
	cClass = header1 & 126976;
	byteSize = (header1 & 252) + sizeHiBits;
	format = (((unsigned) header1) >> 8) & 15;
	if (format < 8) {
		inc = size * 4;
	} else {
		inc = (size + 3) & 4294967292U;
		binc = 3 - ((size + 3) & 3);
		header1 = header1 | (binc << 8);
	}
	if ((byteSize + inc) > 255) {
		header3 = byteSize + inc;
		header1 -= byteSize & 255;
	} else {
		header1 += inc;
	}
	byteSize += inc;
	if (header3 > 0) {
		hdrSize = 3;
	} else {
		if (cClass == 0) {
			hdrSize = 2;
		} else {
			hdrSize = 1;
		}
	}
	if (format <= 4) {
		fillWord = nilObj;
	} else {
		fillWord = 0;
	}
	/* begin allocate:headerSize:h1:h2:h3:doFill:with: */
	if (hdrSize > 1) {
		/* begin pushRemappableOop: */
		remapBuffer[remapBufferCount += 1] = header2;
	}
	/* begin allocateChunk: */
	if (allocationCount >= allocationsBetweenGCs) {
		incrementalGC();
	}
	/* begin sufficientSpaceToAllocate: */
	minFree = (lowSpaceThreshold + (byteSize + ((hdrSize - 1) * 4))) + 4;
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
		enoughSpace = 1;
		goto l1;
	} else {
		enoughSpace = sufficientSpaceAfterGC(minFree);
		goto l1;
	}
l1:	/* end sufficientSpaceToAllocate: */;
	if (!(enoughSpace)) {
		signalLowSpace = 1;
		lowSpaceThreshold = 0;
		interruptCheckCounter = 0;
	}
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) < (((unsigned ) ((byteSize + ((hdrSize - 1) * 4)) + 4)))) {
		error("out of memory");
	}
	newFreeSize = ((longAt(freeBlock)) & 4294967292U) - (byteSize + ((hdrSize - 1) * 4));
	newChunk = freeBlock;
	freeBlock += byteSize + ((hdrSize - 1) * 4);
	/* begin setSizeOfFree:to: */
	longAtput(freeBlock, (newFreeSize & 4294967292U) | 2);
	allocationCount += 1;
	newObj1 = newChunk;
	if (hdrSize > 1) {
		/* begin popRemappableOop */
		oop = remapBuffer[remapBufferCount];
		remapBufferCount -= 1;
		remappedClassOop = oop;
	}
	if (hdrSize == 3) {
		longAtput(newObj1, header3 | 0);
		longAtput(newObj1 + 4, remappedClassOop | 0);
		longAtput(newObj1 + 8, header1 | 0);
		newObj1 += 8;
	}
	if (hdrSize == 2) {
		longAtput(newObj1, remappedClassOop | 1);
		longAtput(newObj1 + 4, header1 | 1);
		newObj1 += 4;
	}
	if (hdrSize == 1) {
		longAtput(newObj1, header1 | 3);
	}
		end = newObj1 + byteSize;
	i = newObj1 + 4;
	while (i < end) {
		longAtput(i, fillWord);
		i += 4;
	}
;
	;
	newObj = newObj1;
	return newObj;
}

int instantiateContextsizeInBytes(int classPointer, int sizeInBytes) {
    int header1;
    int header2;
    int hdrSize;
    int hash;
    int i;
    int newObj;
    int remappedClassOop;
    int end;
    int oop;
    int newFreeSize;
    int enoughSpace;
    int newChunk;
    int minFree;

	/* begin newObjectHash */
	lastHash = (13849 + (27181 * lastHash)) & 65535;
	hash = lastHash;
	header1 = ((hash << 17) & 536739840) | ((longAt(((((char *) classPointer)) + 4) + (2 << 2))) - 1);
	header1 += sizeInBytes - (header1 & 252);
	header2 = classPointer;
	if ((header1 & 126976) == 0) {
		hdrSize = 2;
	} else {
		hdrSize = 1;
	}
	/* begin allocate:headerSize:h1:h2:h3:doFill:with: */
	if (hdrSize > 1) {
		/* begin pushRemappableOop: */
		remapBuffer[remapBufferCount += 1] = header2;
	}
	/* begin allocateChunk: */
	if (allocationCount >= allocationsBetweenGCs) {
		incrementalGC();
	}
	/* begin sufficientSpaceToAllocate: */
	minFree = (lowSpaceThreshold + (sizeInBytes + ((hdrSize - 1) * 4))) + 4;
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
		enoughSpace = 1;
		goto l1;
	} else {
		enoughSpace = sufficientSpaceAfterGC(minFree);
		goto l1;
	}
l1:	/* end sufficientSpaceToAllocate: */;
	if (!(enoughSpace)) {
		signalLowSpace = 1;
		lowSpaceThreshold = 0;
		interruptCheckCounter = 0;
	}
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) < (((unsigned ) ((sizeInBytes + ((hdrSize - 1) * 4)) + 4)))) {
		error("out of memory");
	}
	newFreeSize = ((longAt(freeBlock)) & 4294967292U) - (sizeInBytes + ((hdrSize - 1) * 4));
	newChunk = freeBlock;
	freeBlock += sizeInBytes + ((hdrSize - 1) * 4);
	/* begin setSizeOfFree:to: */
	longAtput(freeBlock, (newFreeSize & 4294967292U) | 2);
	allocationCount += 1;
	newObj = newChunk;
	if (hdrSize > 1) {
		/* begin popRemappableOop */
		oop = remapBuffer[remapBufferCount];
		remapBufferCount -= 1;
		remappedClassOop = oop;
	}
	if (hdrSize == 3) {
		longAtput(newObj, 0 | 0);
		longAtput(newObj + 4, remappedClassOop | 0);
		longAtput(newObj + 8, header1 | 0);
		newObj += 8;
	}
	if (hdrSize == 2) {
		longAtput(newObj, remappedClassOop | 1);
		longAtput(newObj + 4, header1 | 1);
		newObj += 4;
	}
	if (hdrSize == 1) {
		longAtput(newObj, header1 | 3);
	}
	;
	;
	return newObj;
}

int instantiateSmallClasssizeInBytesfill(int classPointer, int sizeInBytes, int fillValue) {
    int header1;
    int header2;
    int hdrSize;
    int hash;
    int i;
    int newObj;
    int remappedClassOop;
    int end;
    int oop;
    int newFreeSize;
    int enoughSpace;
    int newChunk;
    int minFree;

	/* begin newObjectHash */
	lastHash = (13849 + (27181 * lastHash)) & 65535;
	hash = lastHash;
	header1 = ((hash << 17) & 536739840) | ((longAt(((((char *) classPointer)) + 4) + (2 << 2))) - 1);
	header1 += sizeInBytes - (header1 & 252);
	header2 = classPointer;
	if ((header1 & 126976) == 0) {
		hdrSize = 2;
	} else {
		hdrSize = 1;
	}
	/* begin allocate:headerSize:h1:h2:h3:doFill:with: */
	if (hdrSize > 1) {
		/* begin pushRemappableOop: */
		remapBuffer[remapBufferCount += 1] = header2;
	}
	/* begin allocateChunk: */
	if (allocationCount >= allocationsBetweenGCs) {
		incrementalGC();
	}
	/* begin sufficientSpaceToAllocate: */
	minFree = (lowSpaceThreshold + (sizeInBytes + ((hdrSize - 1) * 4))) + 4;
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
		enoughSpace = 1;
		goto l1;
	} else {
		enoughSpace = sufficientSpaceAfterGC(minFree);
		goto l1;
	}
l1:	/* end sufficientSpaceToAllocate: */;
	if (!(enoughSpace)) {
		signalLowSpace = 1;
		lowSpaceThreshold = 0;
		interruptCheckCounter = 0;
	}
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) < (((unsigned ) ((sizeInBytes + ((hdrSize - 1) * 4)) + 4)))) {
		error("out of memory");
	}
	newFreeSize = ((longAt(freeBlock)) & 4294967292U) - (sizeInBytes + ((hdrSize - 1) * 4));
	newChunk = freeBlock;
	freeBlock += sizeInBytes + ((hdrSize - 1) * 4);
	/* begin setSizeOfFree:to: */
	longAtput(freeBlock, (newFreeSize & 4294967292U) | 2);
	allocationCount += 1;
	newObj = newChunk;
	if (hdrSize > 1) {
		/* begin popRemappableOop */
		oop = remapBuffer[remapBufferCount];
		remapBufferCount -= 1;
		remappedClassOop = oop;
	}
	if (hdrSize == 3) {
		longAtput(newObj, 0 | 0);
		longAtput(newObj + 4, remappedClassOop | 0);
		longAtput(newObj + 8, header1 | 0);
		newObj += 8;
	}
	if (hdrSize == 2) {
		longAtput(newObj, remappedClassOop | 1);
		longAtput(newObj + 4, header1 | 1);
		newObj += 4;
	}
	if (hdrSize == 1) {
		longAtput(newObj, header1 | 3);
	}
		end = newObj + sizeInBytes;
	i = newObj + 4;
	while (i < end) {
		longAtput(i, fillValue);
		i += 4;
	}
;
	;
	return newObj;
}

int integerObjectOf(int value) {
	if (value < 0) {
		return ((2147483648U + value) << 1) + 1;
	} else {
		return (value << 1) + 1;
	}
}

int integerValueOf(int objectPointer) {
	if ((objectPointer & 2147483648U) != 0) {
		return ((((unsigned) (objectPointer & 2147483647U)) >> 1) - 1073741823) - 1;
	} else {
		return ((unsigned) objectPointer) >> 1;
	}
}

void interpret(void) {
    int localTP;
    int localCP;
    int localHomeContext;
    char * localSP;
    char * localIP;
    int currentBytecode;
    int t1;
    int t2;
    int t3;
    int t4;
    int t5;
    int t6;
    int t7;
    int t8;
    int t9;
    int t10;
    int t11;
    int t12;
    int t13;
    int t14;
    int t15;
    int t16;
    int t17;
    int t18;
    int t19;
    int t20;
    int t21;
    int t22;
    int t23;
    int t24;
    int t25;
    int t26;
    int t27;
    int t28;
    int t29;
    int t30;
    int t31;
    int t32;
    int t33;
    int t34;

	/* begin internalizeIPandSP */
	localIP = ((char *) instructionPointer);
	localSP = ((char *) stackPointer);
	localHomeContext = theHomeContext;
	currentBytecode = byteAt(++localIP);
	while (1) {
		switch (currentBytecode) {
		case 0:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((0 & 15) << 2)));
			break;
		case 1:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((1 & 15) << 2)));
			break;
		case 2:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((2 & 15) << 2)));
			break;
		case 3:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((3 & 15) << 2)));
			break;
		case 4:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((4 & 15) << 2)));
			break;
		case 5:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((5 & 15) << 2)));
			break;
		case 6:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((6 & 15) << 2)));
			break;
		case 7:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((7 & 15) << 2)));
			break;
		case 8:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((8 & 15) << 2)));
			break;
		case 9:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((9 & 15) << 2)));
			break;
		case 10:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((10 & 15) << 2)));
			break;
		case 11:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((11 & 15) << 2)));
			break;
		case 12:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((12 & 15) << 2)));
			break;
		case 13:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((13 & 15) << 2)));
			break;
		case 14:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((14 & 15) << 2)));
			break;
		case 15:
			/* pushReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + ((15 & 15) << 2)));
			break;
		case 16:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((16 & 15) + 6) << 2)));
			break;
		case 17:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((17 & 15) + 6) << 2)));
			break;
		case 18:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((18 & 15) + 6) << 2)));
			break;
		case 19:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((19 & 15) + 6) << 2)));
			break;
		case 20:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((20 & 15) + 6) << 2)));
			break;
		case 21:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((21 & 15) + 6) << 2)));
			break;
		case 22:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((22 & 15) + 6) << 2)));
			break;
		case 23:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((23 & 15) + 6) << 2)));
			break;
		case 24:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((24 & 15) + 6) << 2)));
			break;
		case 25:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((25 & 15) + 6) << 2)));
			break;
		case 26:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((26 & 15) + 6) << 2)));
			break;
		case 27:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((27 & 15) + 6) << 2)));
			break;
		case 28:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((28 & 15) + 6) << 2)));
			break;
		case 29:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((29 & 15) + 6) << 2)));
			break;
		case 30:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((30 & 15) + 6) << 2)));
			break;
		case 31:
			/* pushTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + (((31 & 15) + 6) << 2)));
			break;
		case 32:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((32 & 31) + 1) << 2)));
			break;
		case 33:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((33 & 31) + 1) << 2)));
			break;
		case 34:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((34 & 31) + 1) << 2)));
			break;
		case 35:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((35 & 31) + 1) << 2)));
			break;
		case 36:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((36 & 31) + 1) << 2)));
			break;
		case 37:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((37 & 31) + 1) << 2)));
			break;
		case 38:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((38 & 31) + 1) << 2)));
			break;
		case 39:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((39 & 31) + 1) << 2)));
			break;
		case 40:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((40 & 31) + 1) << 2)));
			break;
		case 41:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((41 & 31) + 1) << 2)));
			break;
		case 42:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((42 & 31) + 1) << 2)));
			break;
		case 43:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((43 & 31) + 1) << 2)));
			break;
		case 44:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((44 & 31) + 1) << 2)));
			break;
		case 45:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((45 & 31) + 1) << 2)));
			break;
		case 46:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((46 & 31) + 1) << 2)));
			break;
		case 47:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((47 & 31) + 1) << 2)));
			break;
		case 48:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((48 & 31) + 1) << 2)));
			break;
		case 49:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((49 & 31) + 1) << 2)));
			break;
		case 50:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((50 & 31) + 1) << 2)));
			break;
		case 51:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((51 & 31) + 1) << 2)));
			break;
		case 52:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((52 & 31) + 1) << 2)));
			break;
		case 53:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((53 & 31) + 1) << 2)));
			break;
		case 54:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((54 & 31) + 1) << 2)));
			break;
		case 55:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((55 & 31) + 1) << 2)));
			break;
		case 56:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((56 & 31) + 1) << 2)));
			break;
		case 57:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((57 & 31) + 1) << 2)));
			break;
		case 58:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((58 & 31) + 1) << 2)));
			break;
		case 59:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((59 & 31) + 1) << 2)));
			break;
		case 60:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((60 & 31) + 1) << 2)));
			break;
		case 61:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((61 & 31) + 1) << 2)));
			break;
		case 62:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((62 & 31) + 1) << 2)));
			break;
		case 63:
			/* pushLiteralConstantBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) method)) + 4) + (((63 & 31) + 1) << 2)));
			break;
		case 64:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((64 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 65:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((65 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 66:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((66 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 67:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((67 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 68:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((68 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 69:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((69 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 70:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((70 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 71:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((71 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 72:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((72 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 73:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((73 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 74:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((74 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 75:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((75 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 76:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((76 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 77:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((77 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 78:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((78 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 79:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((79 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 80:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((80 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 81:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((81 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 82:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((82 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 83:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((83 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 84:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((84 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 85:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((85 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 86:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((86 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 87:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((87 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 88:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((88 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 89:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((89 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 90:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((90 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 91:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((91 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 92:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((92 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 93:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((93 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 94:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((94 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 95:
			/* pushLiteralVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + (((95 & 31) + 1) << 2))))) + 4) + (1 << 2)));
			break;
		case 96:
			/* storeAndPopReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			t2 = receiver;
			t1 = longAt(localSP);
			if (t2 < youngStart) {
				possibleRootStoreIntovalue(t2, t1);
			}
			longAtput(((((char *) t2)) + 4) + ((96 & 7) << 2), t1);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 97:
			/* storeAndPopReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			t2 = receiver;
			t1 = longAt(localSP);
			if (t2 < youngStart) {
				possibleRootStoreIntovalue(t2, t1);
			}
			longAtput(((((char *) t2)) + 4) + ((97 & 7) << 2), t1);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 98:
			/* storeAndPopReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			t2 = receiver;
			t1 = longAt(localSP);
			if (t2 < youngStart) {
				possibleRootStoreIntovalue(t2, t1);
			}
			longAtput(((((char *) t2)) + 4) + ((98 & 7) << 2), t1);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 99:
			/* storeAndPopReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			t2 = receiver;
			t1 = longAt(localSP);
			if (t2 < youngStart) {
				possibleRootStoreIntovalue(t2, t1);
			}
			longAtput(((((char *) t2)) + 4) + ((99 & 7) << 2), t1);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 100:
			/* storeAndPopReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			t2 = receiver;
			t1 = longAt(localSP);
			if (t2 < youngStart) {
				possibleRootStoreIntovalue(t2, t1);
			}
			longAtput(((((char *) t2)) + 4) + ((100 & 7) << 2), t1);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 101:
			/* storeAndPopReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			t2 = receiver;
			t1 = longAt(localSP);
			if (t2 < youngStart) {
				possibleRootStoreIntovalue(t2, t1);
			}
			longAtput(((((char *) t2)) + 4) + ((101 & 7) << 2), t1);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 102:
			/* storeAndPopReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			t2 = receiver;
			t1 = longAt(localSP);
			if (t2 < youngStart) {
				possibleRootStoreIntovalue(t2, t1);
			}
			longAtput(((((char *) t2)) + 4) + ((102 & 7) << 2), t1);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 103:
			/* storeAndPopReceiverVariableBytecode */
			currentBytecode = byteAt(++localIP);
			t2 = receiver;
			t1 = longAt(localSP);
			if (t2 < youngStart) {
				possibleRootStoreIntovalue(t2, t1);
			}
			longAtput(((((char *) t2)) + 4) + ((103 & 7) << 2), t1);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 104:
			/* storeAndPopTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(((((char *) localHomeContext)) + 4) + (((104 & 7) + 6) << 2), longAt(localSP));
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 105:
			/* storeAndPopTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(((((char *) localHomeContext)) + 4) + (((105 & 7) + 6) << 2), longAt(localSP));
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 106:
			/* storeAndPopTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(((((char *) localHomeContext)) + 4) + (((106 & 7) + 6) << 2), longAt(localSP));
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 107:
			/* storeAndPopTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(((((char *) localHomeContext)) + 4) + (((107 & 7) + 6) << 2), longAt(localSP));
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 108:
			/* storeAndPopTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(((((char *) localHomeContext)) + 4) + (((108 & 7) + 6) << 2), longAt(localSP));
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 109:
			/* storeAndPopTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(((((char *) localHomeContext)) + 4) + (((109 & 7) + 6) << 2), longAt(localSP));
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 110:
			/* storeAndPopTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(((((char *) localHomeContext)) + 4) + (((110 & 7) + 6) << 2), longAt(localSP));
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 111:
			/* storeAndPopTemporaryVariableBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(((((char *) localHomeContext)) + 4) + (((111 & 7) + 6) << 2), longAt(localSP));
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 112:
			/* pushReceiverBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, receiver);
			break;
		case 113:
			/* pushConstantTrueBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, trueObj);
			break;
		case 114:
			/* pushConstantFalseBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, falseObj);
			break;
		case 115:
			/* pushConstantNilBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, nilObj);
			break;
		case 116:
			/* pushConstantMinusOneBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, 4294967295U);
			break;
		case 117:
			/* pushConstantZeroBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, 1);
			break;
		case 118:
			/* pushConstantOneBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, 3);
			break;
		case 119:
			/* pushConstantTwoBytecode */
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, 5);
			break;
		case 120:
			/* returnReceiver */
			t2 = longAt(((((char *) localHomeContext)) + 4) + (0 << 2));
			t1 = receiver;
			/* begin returnValue:to: */
		commonReturn:	/*  */;
			t3 = nilObj;
			t4 = activeContext;
			if ((t2 == t3) || ((longAt(((((char *) t2)) + 4) + (1 << 2))) == t3)) {
				longAtput(localSP += 4, activeContext);
				longAtput(localSP += 4, t1);
				messageSelector = longAt(((((char *) specialObjectsOop)) + 4) + (21 << 2));
				argumentCount = 1;
				/* begin normalSend */
				goto commonSend;
			l1:	/* end fetchClassOf: */;
			l2:	/* end lookupInMethodCacheSel:class: */;
			l3:	/* end internalExecuteNewMethod */;
			}
			while (!(t4 == t2)) {
				t5 = longAt(((((char *) t4)) + 4) + (0 << 2));
				longAtput(((((char *) t4)) + 4) + (0 << 2), t3);
				longAtput(((((char *) t4)) + 4) + (1 << 2), t3);
				if (reclaimableContextCount > 0) {
					reclaimableContextCount -= 1;
					/* begin recycleContextIfPossible: */
					if ((t4 >= youngStart) && (((((unsigned) (longAt(t4))) >> 12) & 31) == 14)) {
						longAtput(((((char *) t4)) + 4) + (0 << 2), freeContexts);
						freeContexts = t4;
					}
				}
				t4 = t5;
			}
			activeContext = t4;
			if (t4 < youngStart) {
				beRootIfOld(t4);
			}
			/* begin internalFetchContextRegisters: */
			t7 = longAt(((((char *) t4)) + 4) + (3 << 2));
			if ((t7 & 1)) {
				t7 = longAt(((((char *) t4)) + 4) + (5 << 2));
				if (t7 < youngStart) {
					beRootIfOld(t7);
				}
			} else {
				t7 = t4;
			}
			localHomeContext = t7;
			receiver = longAt(((((char *) t7)) + 4) + (5 << 2));
			method = longAt(((((char *) t7)) + 4) + (3 << 2));
			t7 = ((longAt(((((char *) t4)) + 4) + (1 << 2))) >> 1);
			localIP = ((char *) (((method + t7) + 4) - 2));
			t7 = ((longAt(((((char *) t4)) + 4) + (2 << 2))) >> 1);
			localSP = ((char *) ((t4 + 4) + (((6 + t7) - 1) * 4)));
			currentBytecode = byteAt(++localIP);
			longAtput(localSP += 4, t1);
		l5:	/* end returnValue:to: */;
			break;
		case 121:
			/* returnTrue */
			t2 = longAt(((((char *) localHomeContext)) + 4) + (0 << 2));
			t1 = trueObj;
			/* begin returnValue:to: */
			goto commonReturn;
		l10:	/* end returnValue:to: */;
			break;
		case 122:
			/* returnFalse */
			t2 = longAt(((((char *) localHomeContext)) + 4) + (0 << 2));
			t1 = falseObj;
			/* begin returnValue:to: */
			goto commonReturn;
		l15:	/* end returnValue:to: */;
			break;
		case 123:
			/* returnNil */
			t2 = longAt(((((char *) localHomeContext)) + 4) + (0 << 2));
			t1 = nilObj;
			/* begin returnValue:to: */
			goto commonReturn;
		l20:	/* end returnValue:to: */;
			break;
		case 124:
			/* returnTopFromMethod */
			t2 = longAt(((((char *) localHomeContext)) + 4) + (0 << 2));
			t1 = longAt(localSP);
			/* begin returnValue:to: */
			goto commonReturn;
		l25:	/* end returnValue:to: */;
			break;
		case 125:
			/* returnTopFromBlock */
			t2 = longAt(((((char *) activeContext)) + 4) + (0 << 2));
			t1 = longAt(localSP);
			/* begin returnValue:to: */
			goto commonReturn;
		l30:	/* end returnValue:to: */;
			break;
		case 126:
		case 127:
			/* unknownBytecode */
			error("Unknown bytecode");
			break;
		case 128:
			/* extendedPushBytecode */
			t1 = byteAt(++localIP);
			currentBytecode = byteAt(++localIP);
			t2 = (((unsigned) t1) >> 6) & 3;
			t3 = t1 & 63;
			if (t2 == 0) {
				longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + (t3 << 2)));
				goto l31;
			}
			if (t2 == 1) {
				longAtput(localSP += 4, longAt(((((char *) localHomeContext)) + 4) + ((t3 + 6) << 2)));
				goto l31;
			}
			if (t2 == 2) {
				longAtput(localSP += 4, longAt(((((char *) method)) + 4) + ((t3 + 1) << 2)));
				goto l31;
			}
			if (t2 == 3) {
				longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + ((t3 + 1) << 2))))) + 4) + (1 << 2)));
				goto l31;
			}
		l31:	/* end case */;
			break;
		case 129:
			/* extendedStoreBytecode */
			t1 = byteAt(++localIP);
			currentBytecode = byteAt(++localIP);
			t2 = (((unsigned) t1) >> 6) & 3;
			t3 = t1 & 63;
			if (t2 == 0) {
				/* begin storePointer:ofObject:withValue: */
				t5 = receiver;
				t6 = longAt(localSP);
				if (t5 < youngStart) {
					possibleRootStoreIntovalue(t5, t6);
				}
				longAtput(((((char *) t5)) + 4) + (t3 << 2), t6);
				goto l32;
			}
			if (t2 == 1) {
				longAtput(((((char *) localHomeContext)) + 4) + ((t3 + 6) << 2), longAt(localSP));
				goto l32;
			}
			if (t2 == 2) {
				error("illegal store");
			}
			if (t2 == 3) {
				t4 = longAt(((((char *) method)) + 4) + ((t3 + 1) << 2));
				/* begin storePointer:ofObject:withValue: */
				t7 = longAt(localSP);
				if (t4 < youngStart) {
					possibleRootStoreIntovalue(t4, t7);
				}
				longAtput(((((char *) t4)) + 4) + (1 << 2), t7);
				goto l32;
			}
		l32:	/* end case */;
			break;
		case 130:
			/* extendedStoreAndPopBytecode */
			/* begin extendedStoreBytecode */
			t1 = byteAt(++localIP);
			currentBytecode = byteAt(++localIP);
			t2 = (((unsigned) t1) >> 6) & 3;
			t3 = t1 & 63;
			if (t2 == 0) {
				/* begin storePointer:ofObject:withValue: */
				t5 = receiver;
				t6 = longAt(localSP);
				if (t5 < youngStart) {
					possibleRootStoreIntovalue(t5, t6);
				}
				longAtput(((((char *) t5)) + 4) + (t3 << 2), t6);
				goto l33;
			}
			if (t2 == 1) {
				longAtput(((((char *) localHomeContext)) + 4) + ((t3 + 6) << 2), longAt(localSP));
				goto l33;
			}
			if (t2 == 2) {
				error("illegal store");
			}
			if (t2 == 3) {
				t4 = longAt(((((char *) method)) + 4) + ((t3 + 1) << 2));
				/* begin storePointer:ofObject:withValue: */
				t7 = longAt(localSP);
				if (t4 < youngStart) {
					possibleRootStoreIntovalue(t4, t7);
				}
				longAtput(((((char *) t4)) + 4) + (1 << 2), t7);
				goto l33;
			}
		l33:	/* end extendedStoreBytecode */;
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 131:
			/* singleExtendedSendBytecode */
			t1 = byteAt(++localIP);
			messageSelector = longAt(((((char *) method)) + 4) + (((t1 & 31) + 1) << 2));
			argumentCount = ((unsigned) t1) >> 5;
			/* begin normalSend */
		commonSend:	/*  */;
			t2 = longAt(localSP - (argumentCount * 4));
			/* begin fetchClassOf: */
			if ((t2 & 1)) {
				lkupClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
				goto l34;
			}
			t3 = (((unsigned) (longAt(t2))) >> 12) & 31;
			if (t3 == 0) {
				lkupClass = (longAt(t2 - 4)) & 4294967292U;
				goto l34;
			} else {
				lkupClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((t3 - 1) << 2));
				goto l34;
			}
		l34:	/* end fetchClassOf: */;
		commonLookup:	/*  */;
			/* begin lookupInMethodCacheSel:class: */
			t6 = messageSelector ^ lkupClass;
			t5 = t6 & 1020;
			if (((methodCache[t5 + 1]) == messageSelector) && ((methodCache[t5 + 2]) == lkupClass)) {
				newMethod = methodCache[t5 + 3];
				primitiveIndex = methodCache[t5 + 4];
				t4 = 1;
				goto l35;
			}
			t5 = (((unsigned) t6) >> 1) & 1020;
			if (((methodCache[t5 + 1]) == messageSelector) && ((methodCache[t5 + 2]) == lkupClass)) {
				newMethod = methodCache[t5 + 3];
				primitiveIndex = methodCache[t5 + 4];
				t4 = 1;
				goto l35;
			}
			t5 = (((unsigned) t6) >> 2) & 1020;
			if (((methodCache[t5 + 1]) == messageSelector) && ((methodCache[t5 + 2]) == lkupClass)) {
				newMethod = methodCache[t5 + 3];
				primitiveIndex = methodCache[t5 + 4];
				t4 = 1;
				goto l35;
			}
			t4 = 0;
		l35:	/* end lookupInMethodCacheSel:class: */;
			if (!(t4)) {
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				lookupMethodInClass(lkupClass);
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
				addToMethodCacheSelclassmethodprimIndex(messageSelector, lkupClass, newMethod, primitiveIndex);
			}
			/* begin internalExecuteNewMethod */
			t7 = primitiveIndex;
			if (t7 > 0) {
				if ((t7 > 255) && (t7 < 520)) {
					if (t7 >= 264) {
						/* begin internalPop:thenPush: */
						t8 = longAt(((((char *) (longAt(localSP)))) + 4) + ((t7 - 264) << 2));
						longAtput(localSP -= (1 - 1) * 4, t8);
						goto l36;
					} else {
						if (t7 == 256) {
							goto l36;
						}
						if (t7 == 257) {
							longAtput(localSP -= (1 - 1) * 4, trueObj);
							goto l36;
						}
						if (t7 == 258) {
							longAtput(localSP -= (1 - 1) * 4, falseObj);
							goto l36;
						}
						if (t7 == 259) {
							longAtput(localSP -= (1 - 1) * 4, nilObj);
							goto l36;
						}
						longAtput(localSP -= (1 - 1) * 4, (((t7 - 261) << 1) | 1));
						goto l36;
					}
				} else {
					/* begin externalizeIPandSP */
					instructionPointer = ((int) localIP);
					stackPointer = ((int) localSP);
					theHomeContext = localHomeContext;
					primitiveResponse();
					/* begin internalizeIPandSP */
					localIP = ((char *) instructionPointer);
					localSP = ((char *) stackPointer);
					localHomeContext = theHomeContext;
					if (successFlag) {
						goto l36;
					}
				}
			}
			/* begin internalBytecodeActivateNewMethod */
			t11 = longAt(((((char *) newMethod)) + 4) + (0 << 2));
			if (freeContexts != 1) {
				t13 = freeContexts;
				freeContexts = longAt(((((char *) t13)) + 4) + (0 << 2));
			} else {
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				/* begin allocateOrRecycleContext */
				if (freeContexts != 1) {
					t14 = freeContexts;
					freeContexts = longAt(((((char *) t14)) + 4) + (0 << 2));
					t13 = t14;
					goto l37;
				}
				t14 = instantiateContextsizeInBytes(longAt(((((char *) specialObjectsOop)) + 4) + (10 << 2)), 156);
				longAtput(((((char *) t14)) + 4) + (4 << 2), nilObj);
				t13 = t14;
			l37:	/* end allocateOrRecycleContext */;
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
			}
			t12 = (((unsigned) t11) >> 19) & 63;
			longAtput(((((char *) t13)) + 4) + (0 << 2), activeContext);
			longAtput(((((char *) t13)) + 4) + (1 << 2), (((((1 + ((((unsigned) t11) >> 10) & 255)) * 4) + 1) << 1) | 1));
			longAtput(((((char *) t13)) + 4) + (2 << 2), ((t12 << 1) | 1));
			longAtput(((((char *) t13)) + 4) + (3 << 2), newMethod);
			t9 = argumentCount;
			for (t10 = 0; t10 <= t9; t10 += 1) {
				longAtput(((((char *) t13)) + 4) + ((5 + t10) << 2), longAt(localSP - ((t9 - t10) * 4)));
			}
			t11 = nilObj;
			for (t10 = (t9 + 1); t10 <= t12; t10 += 1) {
				longAtput(((((char *) t13)) + 4) + ((5 + t10) << 2), t11);
			}
			/* begin internalPop: */
			localSP -= (t9 + 1) * 4;
			reclaimableContextCount += 1;
			/* begin internalNewActiveContext: */
			/* begin internalStoreContextRegisters: */
			longAtput(((((char *) activeContext)) + 4) + (1 << 2), (((((((int) localIP )) + 2) - (method + 4)) << 1) | 1));
			longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((((int) localSP )) - (activeContext + 4))) >> 2) - 6) + 1) << 1) | 1));
			if (t13 < youngStart) {
				beRootIfOld(t13);
			}
			activeContext = t13;
			/* begin internalFetchContextRegisters: */
			t15 = longAt(((((char *) t13)) + 4) + (3 << 2));
			if ((t15 & 1)) {
				t15 = longAt(((((char *) t13)) + 4) + (5 << 2));
				if (t15 < youngStart) {
					beRootIfOld(t15);
				}
			} else {
				t15 = t13;
			}
			localHomeContext = t15;
			receiver = longAt(((((char *) t15)) + 4) + (5 << 2));
			method = longAt(((((char *) t15)) + 4) + (3 << 2));
			t15 = ((longAt(((((char *) t13)) + 4) + (1 << 2))) >> 1);
			localIP = ((char *) (((method + t15) + 4) - 2));
			t15 = ((longAt(((((char *) t13)) + 4) + (2 << 2))) >> 1);
			localSP = ((char *) ((t13 + 4) + (((6 + t15) - 1) * 4)));
			/* begin internalQuickCheckForInterrupts */
			if ((interruptCheckCounter -= 1) <= 0) {
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				checkForInterrupts();
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
			}
		l36:	/* end internalExecuteNewMethod */;
			currentBytecode = byteAt(++localIP);
			break;
		case 132:
			/* doubleExtendedDoAnythingBytecode */
			t1 = byteAt(++localIP);
			t2 = byteAt(++localIP);
			t4 = ((unsigned) t1) >> 5;
			if (t4 == 0) {
				messageSelector = longAt(((((char *) method)) + 4) + ((t2 + 1) << 2));
				argumentCount = t1 & 31;
				/* begin normalSend */
				goto commonSend;
			l40:	/* end fetchClassOf: */;
			l39:	/* end lookupInMethodCacheSel:class: */;
			l41:	/* end internalExecuteNewMethod */;
			}
			if (t4 == 1) {
				messageSelector = longAt(((((char *) method)) + 4) + ((t2 + 1) << 2));
				argumentCount = t1 & 31;
				/* begin superclassSend */
				goto commonSupersend;
			l42:	/* end lookupInMethodCacheSel:class: */;
			l45:	/* end internalExecuteNewMethod */;
			}
			currentBytecode = byteAt(++localIP);
			if (t4 == 2) {
				longAtput(localSP += 4, longAt(((((char *) receiver)) + 4) + (t2 << 2)));
				goto l38;
			}
			if (t4 == 3) {
				longAtput(localSP += 4, longAt(((((char *) method)) + 4) + ((t2 + 1) << 2)));
				goto l38;
			}
			if (t4 == 4) {
				longAtput(localSP += 4, longAt(((((char *) (longAt(((((char *) method)) + 4) + ((t2 + 1) << 2))))) + 4) + (1 << 2)));
				goto l38;
			}
			if (t4 == 5) {
				t3 = longAt(localSP);
				/* begin storePointer:ofObject:withValue: */
				t16 = receiver;
				if (t16 < youngStart) {
					possibleRootStoreIntovalue(t16, t3);
				}
				longAtput(((((char *) t16)) + 4) + (t2 << 2), t3);
				goto l38;
			}
			if (t4 == 6) {
				t3 = longAt(localSP);
				/* begin internalPop: */
				localSP -= 1 * 4;
				/* begin storePointer:ofObject:withValue: */
				t17 = receiver;
				if (t17 < youngStart) {
					possibleRootStoreIntovalue(t17, t3);
				}
				longAtput(((((char *) t17)) + 4) + (t2 << 2), t3);
				goto l38;
			}
			if (t4 == 7) {
				t3 = longAt(localSP);
				/* begin storePointer:ofObject:withValue: */
				t18 = longAt(((((char *) method)) + 4) + ((t2 + 1) << 2));
				if (t18 < youngStart) {
					possibleRootStoreIntovalue(t18, t3);
				}
				longAtput(((((char *) t18)) + 4) + (1 << 2), t3);
				goto l38;
			}
		l38:	/* end case */;
			break;
		case 133:
			/* singleExtendedSuperBytecode */
			t1 = byteAt(++localIP);
			messageSelector = longAt(((((char *) method)) + 4) + (((t1 & 31) + 1) << 2));
			argumentCount = ((unsigned) t1) >> 5;
			/* begin superclassSend */
		commonSupersend:	/*  */;
			/* begin superclassOf: */
			t2 = longAt(((((char *) (longAt(((((char *) method)) + 4) + (((((((unsigned) (longAt(((((char *) method)) + 4) + (0 << 2)))) >> 10) & 255) - 1) + 1) << 2))))) + 4) + (1 << 2));
			lkupClass = longAt(((((char *) t2)) + 4) + (0 << 2));
			goto commonLookup;
		l46:	/* end lookupInMethodCacheSel:class: */;
		l48:	/* end internalExecuteNewMethod */;
			break;
		case 134:
			/* secondExtendedSendBytecode */
			t1 = byteAt(++localIP);
			messageSelector = longAt(((((char *) method)) + 4) + (((t1 & 63) + 1) << 2));
			argumentCount = ((unsigned) t1) >> 6;
			/* begin normalSend */
			goto commonSend;
		l50:	/* end fetchClassOf: */;
		l49:	/* end lookupInMethodCacheSel:class: */;
		l51:	/* end internalExecuteNewMethod */;
			break;
		case 135:
			/* popStackBytecode */
			currentBytecode = byteAt(++localIP);
			/* begin internalPop: */
			localSP -= 1 * 4;
			break;
		case 136:
			/* duplicateTopBytecode */
			currentBytecode = byteAt(++localIP);
			/* begin internalPush: */
			t1 = longAt(localSP);
			longAtput(localSP += 4, t1);
			break;
		case 137:
			/* pushActiveContextBytecode */
			currentBytecode = byteAt(++localIP);
			reclaimableContextCount = 0;
			longAtput(localSP += 4, activeContext);
			break;
		case 138:
		case 139:
		case 140:
		case 141:
		case 142:
		case 143:
			/* experimentalBytecode */
			t3 = longAt(((((char *) localHomeContext)) + 4) + (((currentBytecode - 138) + 6) << 2));
			t4 = byteAt(localIP + 1);
			t5 = byteAt(localIP + 2);
			t6 = byteAt(localIP + 3);
			if ((t3 & 1)) {
				t1 = (t3 >> 1);
			} else {
				currentBytecode = byteAt(++localIP);
				longAtput(localSP += 4, t3);
				goto l53;
			}
			if (t4 < 32) {
				t2 = longAt(((((char *) localHomeContext)) + 4) + (((t4 & 15) + 6) << 2));
				if ((t2 & 1)) {
					t2 = (t2 >> 1);
				} else {
					currentBytecode = byteAt(++localIP);
					longAtput(localSP += 4, t3);
					goto l53;
				}
			} else {
				if (t4 > 64) {
					t2 = 1;
				} else {
					t2 = longAt(((((char *) method)) + 4) + (((t4 & 31) + 1) << 2));
					if ((t2 & 1)) {
						t2 = (t2 >> 1);
					} else {
						currentBytecode = byteAt(++localIP);
						longAtput(localSP += 4, t3);
						goto l53;
					}
				}
			}
			if (t5 < 178) {
				t8 = t1 + t2;
				if ((t8 ^ (t8 << 1)) >= 0) {
					if ((t6 > 103) && (t6 < 112)) {
						localIP += 3;
						longAtput(((((char *) localHomeContext)) + 4) + (((t6 & 7) + 6) << 2), ((t8 << 1) | 1));
					} else {
						localIP += 2;
						longAtput(localSP += 4, ((t8 << 1) | 1));
					}
				} else {
					currentBytecode = byteAt(++localIP);
					longAtput(localSP += 4, t3);
					goto l53;
				}
			} else {
				t7 = byteAt(localIP + 4);
				if (t1 <= t2) {
					localIP = (localIP + 3) + 1;
				} else {
					localIP = ((localIP + 3) + 1) + t7;
				}
				currentBytecode = byteAt(++localIP);
			}
		l53:	/* end case */;
			break;
		case 144:
			/* shortUnconditionalJump */
			/* begin jump: */
			t1 = (144 & 7) + 1;
			localIP = (localIP + t1) + 1;
			currentBytecode = byteAt(localIP);
			break;
		case 145:
			/* shortUnconditionalJump */
			/* begin jump: */
			t1 = (145 & 7) + 1;
			localIP = (localIP + t1) + 1;
			currentBytecode = byteAt(localIP);
			break;
		case 146:
			/* shortUnconditionalJump */
			/* begin jump: */
			t1 = (146 & 7) + 1;
			localIP = (localIP + t1) + 1;
			currentBytecode = byteAt(localIP);
			break;
		case 147:
			/* shortUnconditionalJump */
			/* begin jump: */
			t1 = (147 & 7) + 1;
			localIP = (localIP + t1) + 1;
			currentBytecode = byteAt(localIP);
			break;
		case 148:
			/* shortUnconditionalJump */
			/* begin jump: */
			t1 = (148 & 7) + 1;
			localIP = (localIP + t1) + 1;
			currentBytecode = byteAt(localIP);
			break;
		case 149:
			/* shortUnconditionalJump */
			/* begin jump: */
			t1 = (149 & 7) + 1;
			localIP = (localIP + t1) + 1;
			currentBytecode = byteAt(localIP);
			break;
		case 150:
			/* shortUnconditionalJump */
			/* begin jump: */
			t1 = (150 & 7) + 1;
			localIP = (localIP + t1) + 1;
			currentBytecode = byteAt(localIP);
			break;
		case 151:
			/* shortUnconditionalJump */
			/* begin jump: */
			t1 = (151 & 7) + 1;
			localIP = (localIP + t1) + 1;
			currentBytecode = byteAt(localIP);
			break;
		case 152:
		case 153:
		case 154:
		case 155:
		case 156:
		case 157:
		case 158:
		case 159:
			/* shortConditionalJump */
			/* begin jumplfFalseBy: */
			t1 = (currentBytecode & 7) + 1;
			t2 = longAt(localSP);
			if (t2 == falseObj) {
				/* begin jump: */
				localIP = (localIP + t1) + 1;
				currentBytecode = byteAt(localIP);
			} else {
				if (!(t2 == trueObj)) {
					messageSelector = longAt(((((char *) specialObjectsOop)) + 4) + (25 << 2));
					argumentCount = 0;
					/* begin normalSend */
					goto commonSend;
				l54:	/* end fetchClassOf: */;
				l55:	/* end lookupInMethodCacheSel:class: */;
				l56:	/* end internalExecuteNewMethod */;
				}
				currentBytecode = byteAt(++localIP);
			}
			/* begin internalPop: */
			localSP -= 1 * 4;
		l58:	/* end jumplfFalseBy: */;
			break;
		case 160:
		case 161:
		case 162:
		case 163:
		case 164:
		case 165:
		case 166:
		case 167:
			/* longUnconditionalJump */
			t1 = (((currentBytecode & 7) - 4) * 256) + (byteAt(++localIP));
			localIP += t1;
			if (t1 < 0) {
				/* begin internalQuickCheckForInterrupts */
				if ((interruptCheckCounter -= 1) <= 0) {
					/* begin externalizeIPandSP */
					instructionPointer = ((int) localIP);
					stackPointer = ((int) localSP);
					theHomeContext = localHomeContext;
					checkForInterrupts();
					/* begin internalizeIPandSP */
					localIP = ((char *) instructionPointer);
					localSP = ((char *) stackPointer);
					localHomeContext = theHomeContext;
				}
			}
			currentBytecode = byteAt(++localIP);
			break;
		case 168:
		case 169:
		case 170:
		case 171:
			/* longJumpIfTrue */
			/* begin jumplfTrueBy: */
			t1 = ((currentBytecode & 3) * 256) + (byteAt(++localIP));
			t2 = longAt(localSP);
			if (t2 == trueObj) {
				/* begin jump: */
				localIP = (localIP + t1) + 1;
				currentBytecode = byteAt(localIP);
			} else {
				if (!(t2 == falseObj)) {
					messageSelector = longAt(((((char *) specialObjectsOop)) + 4) + (25 << 2));
					argumentCount = 0;
					/* begin normalSend */
					goto commonSend;
				l60:	/* end fetchClassOf: */;
				l59:	/* end lookupInMethodCacheSel:class: */;
				l61:	/* end internalExecuteNewMethod */;
				}
				currentBytecode = byteAt(++localIP);
			}
			/* begin internalPop: */
			localSP -= 1 * 4;
		l63:	/* end jumplfTrueBy: */;
			break;
		case 172:
		case 173:
		case 174:
		case 175:
			/* longJumpIfFalse */
			/* begin jumplfFalseBy: */
			t1 = ((currentBytecode & 3) * 256) + (byteAt(++localIP));
			t2 = longAt(localSP);
			if (t2 == falseObj) {
				/* begin jump: */
				localIP = (localIP + t1) + 1;
				currentBytecode = byteAt(localIP);
			} else {
				if (!(t2 == trueObj)) {
					messageSelector = longAt(((((char *) specialObjectsOop)) + 4) + (25 << 2));
					argumentCount = 0;
					/* begin normalSend */
					goto commonSend;
				l64:	/* end fetchClassOf: */;
				l65:	/* end lookupInMethodCacheSel:class: */;
				l66:	/* end internalExecuteNewMethod */;
				}
				currentBytecode = byteAt(++localIP);
			}
			/* begin internalPop: */
			localSP -= 1 * 4;
		l68:	/* end jumplfFalseBy: */;
			break;
		case 176:
			/* bytecodePrimAdd */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				t2 = ((t3 >> 1)) + ((t1 >> 1));
				if ((t2 ^ (t2 << 1)) >= 0) {
					longAtput(localSP -= (2 - 1) * 4, ((t2 << 1) | 1));
					currentBytecode = byteAt(++localIP);
					goto l69;
				}
			} else {
				successFlag = 1;
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				primitiveFloatAddtoArg(t3, t1);
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
				if (successFlag) {
					currentBytecode = byteAt(++localIP);
					goto l69;
				}
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((0 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l71:	/* end fetchClassOf: */;
		l70:	/* end lookupInMethodCacheSel:class: */;
		l72:	/* end internalExecuteNewMethod */;
		l69:	/* end case */;
			break;
		case 177:
			/* bytecodePrimSubtract */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				t2 = ((t3 >> 1)) - ((t1 >> 1));
				if ((t2 ^ (t2 << 1)) >= 0) {
					longAtput(localSP -= (2 - 1) * 4, ((t2 << 1) | 1));
					currentBytecode = byteAt(++localIP);
					goto l74;
				}
			} else {
				successFlag = 1;
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				primitiveFloatSubtractfromArg(t3, t1);
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
				if (successFlag) {
					currentBytecode = byteAt(++localIP);
					goto l74;
				}
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((1 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l76:	/* end fetchClassOf: */;
		l75:	/* end lookupInMethodCacheSel:class: */;
		l77:	/* end internalExecuteNewMethod */;
		l74:	/* end case */;
			break;
		case 178:
			/* bytecodePrimLessThan */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				/* begin booleanCheat: */
				t4 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t4 < 160) && (t4 > 151)) {
					if (t3 < t1) {
						currentBytecode = byteAt(++localIP);
						goto l79;
					} else {
						/* begin jump: */
						localIP = (localIP + (t4 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l79;
					}
				}
				if (t4 == 172) {
					t5 = byteAt(++localIP);
					if (t3 < t1) {
						currentBytecode = byteAt(++localIP);
						goto l79;
					} else {
						/* begin jump: */
						localIP = (localIP + t5) + 1;
						currentBytecode = byteAt(localIP);
						goto l79;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t3 < t1) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l79;
			}
			successFlag = 1;
			t2 = primitiveFloatLessthanArg(t3, t1);
			if (successFlag) {
				/* begin booleanCheat: */
				t6 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t6 < 160) && (t6 > 151)) {
					if (t2) {
						currentBytecode = byteAt(++localIP);
						goto l79;
					} else {
						/* begin jump: */
						localIP = (localIP + (t6 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l79;
					}
				}
				if (t6 == 172) {
					t7 = byteAt(++localIP);
					if (t2) {
						currentBytecode = byteAt(++localIP);
						goto l79;
					} else {
						/* begin jump: */
						localIP = (localIP + t7) + 1;
						currentBytecode = byteAt(localIP);
						goto l79;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t2) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l79;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((2 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l81:	/* end fetchClassOf: */;
		l80:	/* end lookupInMethodCacheSel:class: */;
		l82:	/* end internalExecuteNewMethod */;
		l79:	/* end case */;
			break;
		case 179:
			/* bytecodePrimGreaterThan */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				/* begin booleanCheat: */
				t4 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t4 < 160) && (t4 > 151)) {
					if (t3 > t1) {
						currentBytecode = byteAt(++localIP);
						goto l84;
					} else {
						/* begin jump: */
						localIP = (localIP + (t4 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l84;
					}
				}
				if (t4 == 172) {
					t5 = byteAt(++localIP);
					if (t3 > t1) {
						currentBytecode = byteAt(++localIP);
						goto l84;
					} else {
						/* begin jump: */
						localIP = (localIP + t5) + 1;
						currentBytecode = byteAt(localIP);
						goto l84;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t3 > t1) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l84;
			}
			successFlag = 1;
			t2 = primitiveFloatGreaterthanArg(t3, t1);
			if (successFlag) {
				/* begin booleanCheat: */
				t6 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t6 < 160) && (t6 > 151)) {
					if (t2) {
						currentBytecode = byteAt(++localIP);
						goto l84;
					} else {
						/* begin jump: */
						localIP = (localIP + (t6 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l84;
					}
				}
				if (t6 == 172) {
					t7 = byteAt(++localIP);
					if (t2) {
						currentBytecode = byteAt(++localIP);
						goto l84;
					} else {
						/* begin jump: */
						localIP = (localIP + t7) + 1;
						currentBytecode = byteAt(localIP);
						goto l84;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t2) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l84;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((3 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l86:	/* end fetchClassOf: */;
		l85:	/* end lookupInMethodCacheSel:class: */;
		l87:	/* end internalExecuteNewMethod */;
		l84:	/* end case */;
			break;
		case 180:
			/* bytecodePrimLessOrEqual */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				/* begin booleanCheat: */
				t4 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t4 < 160) && (t4 > 151)) {
					if (t3 <= t1) {
						currentBytecode = byteAt(++localIP);
						goto l89;
					} else {
						/* begin jump: */
						localIP = (localIP + (t4 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l89;
					}
				}
				if (t4 == 172) {
					t5 = byteAt(++localIP);
					if (t3 <= t1) {
						currentBytecode = byteAt(++localIP);
						goto l89;
					} else {
						/* begin jump: */
						localIP = (localIP + t5) + 1;
						currentBytecode = byteAt(localIP);
						goto l89;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t3 <= t1) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l89;
			}
			successFlag = 1;
			t2 = primitiveFloatGreaterthanArg(t3, t1);
			if (successFlag) {
				/* begin booleanCheat: */
				t6 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t6 < 160) && (t6 > 151)) {
					if (!t2) {
						currentBytecode = byteAt(++localIP);
						goto l89;
					} else {
						/* begin jump: */
						localIP = (localIP + (t6 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l89;
					}
				}
				if (t6 == 172) {
					t7 = byteAt(++localIP);
					if (!t2) {
						currentBytecode = byteAt(++localIP);
						goto l89;
					} else {
						/* begin jump: */
						localIP = (localIP + t7) + 1;
						currentBytecode = byteAt(localIP);
						goto l89;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (!t2) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l89;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((4 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l91:	/* end fetchClassOf: */;
		l90:	/* end lookupInMethodCacheSel:class: */;
		l92:	/* end internalExecuteNewMethod */;
		l89:	/* end case */;
			break;
		case 181:
			/* bytecodePrimGreaterOrEqual */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				/* begin booleanCheat: */
				t4 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t4 < 160) && (t4 > 151)) {
					if (t3 >= t1) {
						currentBytecode = byteAt(++localIP);
						goto l94;
					} else {
						/* begin jump: */
						localIP = (localIP + (t4 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l94;
					}
				}
				if (t4 == 172) {
					t5 = byteAt(++localIP);
					if (t3 >= t1) {
						currentBytecode = byteAt(++localIP);
						goto l94;
					} else {
						/* begin jump: */
						localIP = (localIP + t5) + 1;
						currentBytecode = byteAt(localIP);
						goto l94;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t3 >= t1) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l94;
			}
			successFlag = 1;
			t2 = primitiveFloatLessthanArg(t3, t1);
			if (successFlag) {
				/* begin booleanCheat: */
				t6 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t6 < 160) && (t6 > 151)) {
					if (!t2) {
						currentBytecode = byteAt(++localIP);
						goto l94;
					} else {
						/* begin jump: */
						localIP = (localIP + (t6 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l94;
					}
				}
				if (t6 == 172) {
					t7 = byteAt(++localIP);
					if (!t2) {
						currentBytecode = byteAt(++localIP);
						goto l94;
					} else {
						/* begin jump: */
						localIP = (localIP + t7) + 1;
						currentBytecode = byteAt(localIP);
						goto l94;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (!t2) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l94;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((5 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l96:	/* end fetchClassOf: */;
		l95:	/* end lookupInMethodCacheSel:class: */;
		l97:	/* end internalExecuteNewMethod */;
		l94:	/* end case */;
			break;
		case 182:
			/* bytecodePrimEqual */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				/* begin booleanCheat: */
				t4 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t4 < 160) && (t4 > 151)) {
					if (t3 == t1) {
						currentBytecode = byteAt(++localIP);
						goto l99;
					} else {
						/* begin jump: */
						localIP = (localIP + (t4 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l99;
					}
				}
				if (t4 == 172) {
					t5 = byteAt(++localIP);
					if (t3 == t1) {
						currentBytecode = byteAt(++localIP);
						goto l99;
					} else {
						/* begin jump: */
						localIP = (localIP + t5) + 1;
						currentBytecode = byteAt(localIP);
						goto l99;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t3 == t1) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l99;
			}
			successFlag = 1;
			t2 = primitiveFloatEqualtoArg(t3, t1);
			if (successFlag) {
				/* begin booleanCheat: */
				t6 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t6 < 160) && (t6 > 151)) {
					if (t2) {
						currentBytecode = byteAt(++localIP);
						goto l99;
					} else {
						/* begin jump: */
						localIP = (localIP + (t6 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l99;
					}
				}
				if (t6 == 172) {
					t7 = byteAt(++localIP);
					if (t2) {
						currentBytecode = byteAt(++localIP);
						goto l99;
					} else {
						/* begin jump: */
						localIP = (localIP + t7) + 1;
						currentBytecode = byteAt(localIP);
						goto l99;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t2) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l99;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((6 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l101:	/* end fetchClassOf: */;
		l100:	/* end lookupInMethodCacheSel:class: */;
		l102:	/* end internalExecuteNewMethod */;
		l99:	/* end case */;
			break;
		case 183:
			/* bytecodePrimNotEqual */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				/* begin booleanCheat: */
				t4 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t4 < 160) && (t4 > 151)) {
					if (t3 != t1) {
						currentBytecode = byteAt(++localIP);
						goto l104;
					} else {
						/* begin jump: */
						localIP = (localIP + (t4 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l104;
					}
				}
				if (t4 == 172) {
					t5 = byteAt(++localIP);
					if (t3 != t1) {
						currentBytecode = byteAt(++localIP);
						goto l104;
					} else {
						/* begin jump: */
						localIP = (localIP + t5) + 1;
						currentBytecode = byteAt(localIP);
						goto l104;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (t3 != t1) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l104;
			}
			successFlag = 1;
			t2 = primitiveFloatEqualtoArg(t3, t1);
			if (successFlag) {
				/* begin booleanCheat: */
				t6 = byteAt(++localIP);
				/* begin internalPop: */
				localSP -= 2 * 4;
				if ((t6 < 160) && (t6 > 151)) {
					if (!t2) {
						currentBytecode = byteAt(++localIP);
						goto l104;
					} else {
						/* begin jump: */
						localIP = (localIP + (t6 - 151)) + 1;
						currentBytecode = byteAt(localIP);
						goto l104;
					}
				}
				if (t6 == 172) {
					t7 = byteAt(++localIP);
					if (!t2) {
						currentBytecode = byteAt(++localIP);
						goto l104;
					} else {
						/* begin jump: */
						localIP = (localIP + t7) + 1;
						currentBytecode = byteAt(localIP);
						goto l104;
					}
				}
				localIP -= 1;
				currentBytecode = byteAt(++localIP);
				if (!t2) {
					longAtput(localSP += 4, trueObj);
				} else {
					longAtput(localSP += 4, falseObj);
				}
				goto l104;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((7 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l106:	/* end fetchClassOf: */;
		l105:	/* end lookupInMethodCacheSel:class: */;
		l107:	/* end internalExecuteNewMethod */;
		l104:	/* end case */;
			break;
		case 184:
			/* bytecodePrimMultiply */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				t3 = (t3 >> 1);
				t1 = (t1 >> 1);
				t2 = t3 * t1;
				if (((t1 == 0) || ((t2 / t1) == t3)) && ((t2 ^ (t2 << 1)) >= 0)) {
					longAtput(localSP -= (2 - 1) * 4, ((t2 << 1) | 1));
					currentBytecode = byteAt(++localIP);
					goto l109;
				}
			} else {
				successFlag = 1;
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				primitiveFloatMultiplybyArg(t3, t1);
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
				if (successFlag) {
					currentBytecode = byteAt(++localIP);
					goto l109;
				}
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((8 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l111:	/* end fetchClassOf: */;
		l110:	/* end lookupInMethodCacheSel:class: */;
		l112:	/* end internalExecuteNewMethod */;
		l109:	/* end case */;
			break;
		case 185:
			/* bytecodePrimDivide */
			t3 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			if (((t3 & t1) & 1) != 0) {
				t3 = (t3 >> 1);
				t1 = (t1 >> 1);
				if ((t1 != 0) && ((t3 % t1) == 0)) {
					t2 = t3 / t1;
					if ((t2 ^ (t2 << 1)) >= 0) {
						longAtput(localSP -= (2 - 1) * 4, ((t2 << 1) | 1));
						currentBytecode = byteAt(++localIP);
						goto l114;
					}
				}
			} else {
				successFlag = 1;
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				primitiveFloatDividebyArg(t3, t1);
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
				if (successFlag) {
					currentBytecode = byteAt(++localIP);
					goto l114;
				}
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((9 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l116:	/* end fetchClassOf: */;
		l115:	/* end lookupInMethodCacheSel:class: */;
		l117:	/* end internalExecuteNewMethod */;
		l114:	/* end case */;
			break;
		case 186:
			/* bytecodePrimMod */
			successFlag = 1;
			t1 = doPrimitiveModby(longAt(localSP - (1 * 4)), longAt(localSP - (0 * 4)));
			if (successFlag) {
				longAtput(localSP -= (2 - 1) * 4, ((t1 << 1) | 1));
				currentBytecode = byteAt(++localIP);
				goto l119;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((10 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l121:	/* end fetchClassOf: */;
		l120:	/* end lookupInMethodCacheSel:class: */;
		l122:	/* end internalExecuteNewMethod */;
		l119:	/* end case */;
			break;
		case 187:
			/* bytecodePrimMakePoint */
			successFlag = 1;
			/* begin externalizeIPandSP */
			instructionPointer = ((int) localIP);
			stackPointer = ((int) localSP);
			theHomeContext = localHomeContext;
			/* begin primitiveMakePoint */
			/* begin popInteger */
			/* begin popStack */
			t7 = longAt(stackPointer);
			stackPointer -= 4;
			t3 = t7;
			if ((t3 & 1)) {
				t2 = (t3 >> 1);
				goto l125;
			} else {
				successFlag = 0;
				t2 = 1;
				goto l125;
			}
		l125:	/* end popInteger */;
			/* begin popInteger */
			/* begin popStack */
			t8 = longAt(stackPointer);
			stackPointer -= 4;
			t4 = t8;
			if ((t4 & 1)) {
				t1 = (t4 >> 1);
				goto l126;
			} else {
				successFlag = 0;
				t1 = 1;
				goto l126;
			}
		l126:	/* end popInteger */;
			if (successFlag) {
				/* begin push: */
				/* begin makePointwithxValue:yValue: */
				t13 = instantiateSmallClasssizeInBytesfill(longAt(((((char *) specialObjectsOop)) + 4) + (12 << 2)), 12, nilObj);
				/* begin storePointer:ofObject:withValue: */
				if (t13 < youngStart) {
					possibleRootStoreIntovalue(t13, ((t1 << 1) | 1));
				}
				longAtput(((((char *) t13)) + 4) + (0 << 2), ((t1 << 1) | 1));
				/* begin storePointer:ofObject:withValue: */
				if (t13 < youngStart) {
					possibleRootStoreIntovalue(t13, ((t2 << 1) | 1));
				}
				longAtput(((((char *) t13)) + 4) + (1 << 2), ((t2 << 1) | 1));
				t5 = t13;
				longAtput(t6 = stackPointer + 4, t5);
				stackPointer = t6;
			} else {
				/* begin unPop: */
				stackPointer += 2 * 4;
			}
			/* begin internalizeIPandSP */
			localIP = ((char *) instructionPointer);
			localSP = ((char *) stackPointer);
			localHomeContext = theHomeContext;
			if (successFlag) {
				currentBytecode = byteAt(++localIP);
				goto l124;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((11 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l128:	/* end fetchClassOf: */;
		l127:	/* end lookupInMethodCacheSel:class: */;
		l129:	/* end internalExecuteNewMethod */;
		l124:	/* end case */;
			break;
		case 188:
			/* bytecodePrimBitShift */
			successFlag = 1;
			/* begin externalizeIPandSP */
			instructionPointer = ((int) localIP);
			stackPointer = ((int) localSP);
			theHomeContext = localHomeContext;
			primitiveBitShift();
			/* begin internalizeIPandSP */
			localIP = ((char *) instructionPointer);
			localSP = ((char *) stackPointer);
			localHomeContext = theHomeContext;
			if (successFlag) {
				currentBytecode = byteAt(++localIP);
				goto l131;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((12 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l133:	/* end fetchClassOf: */;
		l132:	/* end lookupInMethodCacheSel:class: */;
		l134:	/* end internalExecuteNewMethod */;
		l131:	/* end case */;
			break;
		case 189:
			/* bytecodePrimDiv */
			successFlag = 1;
			t1 = doPrimitiveDivby(longAt(localSP - (1 * 4)), longAt(localSP - (0 * 4)));
			if (successFlag) {
				longAtput(localSP -= (2 - 1) * 4, ((t1 << 1) | 1));
				currentBytecode = byteAt(++localIP);
				goto l136;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((13 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l138:	/* end fetchClassOf: */;
		l137:	/* end lookupInMethodCacheSel:class: */;
		l139:	/* end internalExecuteNewMethod */;
		l136:	/* end case */;
			break;
		case 190:
			/* bytecodePrimBitAnd */
			successFlag = 1;
			/* begin externalizeIPandSP */
			instructionPointer = ((int) localIP);
			stackPointer = ((int) localSP);
			theHomeContext = localHomeContext;
			/* begin primitiveBitAnd */
			/* begin popPos32BitInteger */
			/* begin popStack */
			t6 = longAt(stackPointer);
			stackPointer -= 4;
			t5 = t6;
			t2 = positive32BitValueOf(t5);
			/* begin popPos32BitInteger */
			/* begin popStack */
			t8 = longAt(stackPointer);
			stackPointer -= 4;
			t7 = t8;
			t1 = positive32BitValueOf(t7);
			if (successFlag) {
				/* begin push: */
				t3 = positive32BitIntegerFor(t1 & t2);
				longAtput(t4 = stackPointer + 4, t3);
				stackPointer = t4;
			} else {
				/* begin unPop: */
				stackPointer += 2 * 4;
			}
			/* begin internalizeIPandSP */
			localIP = ((char *) instructionPointer);
			localSP = ((char *) stackPointer);
			localHomeContext = theHomeContext;
			if (successFlag) {
				currentBytecode = byteAt(++localIP);
				goto l141;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((14 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l143:	/* end fetchClassOf: */;
		l142:	/* end lookupInMethodCacheSel:class: */;
		l144:	/* end internalExecuteNewMethod */;
		l141:	/* end case */;
			break;
		case 191:
			/* bytecodePrimBitOr */
			successFlag = 1;
			/* begin externalizeIPandSP */
			instructionPointer = ((int) localIP);
			stackPointer = ((int) localSP);
			theHomeContext = localHomeContext;
			/* begin primitiveBitOr */
			/* begin popPos32BitInteger */
			/* begin popStack */
			t6 = longAt(stackPointer);
			stackPointer -= 4;
			t5 = t6;
			t2 = positive32BitValueOf(t5);
			/* begin popPos32BitInteger */
			/* begin popStack */
			t8 = longAt(stackPointer);
			stackPointer -= 4;
			t7 = t8;
			t1 = positive32BitValueOf(t7);
			if (successFlag) {
				/* begin push: */
				t3 = positive32BitIntegerFor(t1 | t2);
				longAtput(t4 = stackPointer + 4, t3);
				stackPointer = t4;
			} else {
				/* begin unPop: */
				stackPointer += 2 * 4;
			}
			/* begin internalizeIPandSP */
			localIP = ((char *) instructionPointer);
			localSP = ((char *) stackPointer);
			localHomeContext = theHomeContext;
			if (successFlag) {
				currentBytecode = byteAt(++localIP);
				goto l146;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((15 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l148:	/* end fetchClassOf: */;
		l147:	/* end lookupInMethodCacheSel:class: */;
		l149:	/* end internalExecuteNewMethod */;
		l146:	/* end case */;
			break;
		case 192:
			/* bytecodePrimAt */
			t1 = longAt(localSP);
			t3 = longAt(localSP - (1 * 4));
			successFlag = (!((t3 & 1))) && ((t1 & 1));
			if (successFlag) {
				t4 = t3 & 28;
				if ((atCache[t4 + 1]) == t3) {
					/* begin commonVariableInternal:at:cacheIndex: */
					t9 = atCache[t4 + 2];
					if (((((unsigned ) ((t1 >> 1)))) >= 1) && ((((unsigned ) ((t1 >> 1)))) <= (((unsigned ) t9)))) {
						t10 = atCache[t4 + 3];
						if (t10 <= 4) {
							t11 = atCache[t4 + 4];
							t2 = longAt(((((char *) t3)) + 4) + (((((t1 >> 1)) + t11) - 1) << 2));
							goto l153;
						}
						if (t10 < 8) {
							t12 = longAt(((((char *) t3)) + 4) + ((((t1 >> 1)) - 1) << 2));
							/* begin externalizeIPandSP */
							instructionPointer = ((int) localIP);
							stackPointer = ((int) localSP);
							theHomeContext = localHomeContext;
							t12 = positive32BitIntegerFor(t12);
							/* begin internalizeIPandSP */
							localIP = ((char *) instructionPointer);
							localSP = ((char *) stackPointer);
							localHomeContext = theHomeContext;
							t2 = t12;
							goto l153;
						}
						if (t10 >= 16) {
							t2 = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (24 << 2))))) + 4) + ((byteAt(((((char *) t3)) + 4) + (((t1 >> 1)) - 1))) << 2));
							goto l153;
						} else {
							t2 = (((byteAt(((((char *) t3)) + 4) + (((t1 >> 1)) - 1))) << 1) | 1);
							goto l153;
						}
					}
					successFlag = 0;
				l153:	/* end commonVariableInternal:at:cacheIndex: */;
					if (successFlag) {
						currentBytecode = byteAt(++localIP);
						longAtput(localSP -= (2 - 1) * 4, t2);
						goto l151;
					}
				}
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((16 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l154:	/* end fetchClassOf: */;
		l152:	/* end lookupInMethodCacheSel:class: */;
		l155:	/* end internalExecuteNewMethod */;
		l151:	/* end case */;
			break;
		case 193:
			/* bytecodePrimAtPut */
			t1 = longAt(localSP);
			t2 = longAt(localSP - (1 * 4));
			t3 = longAt(localSP - (2 * 4));
			successFlag = (!((t3 & 1))) && ((t2 & 1));
			if (successFlag) {
				t4 = (t3 & 28) + 32;
				if ((atCache[t4 + 1]) == t3) {
					/* begin commonVariable:at:put:cacheIndex: */
					t5 = atCache[t4 + 2];
					if (((((unsigned ) ((t2 >> 1)))) >= 1) && ((((unsigned ) ((t2 >> 1)))) <= (((unsigned ) t5)))) {
						t7 = atCache[t4 + 3];
						if (t7 <= 4) {
							t8 = atCache[t4 + 4];
							/* begin storePointer:ofObject:withValue: */
							if (t3 < youngStart) {
								possibleRootStoreIntovalue(t3, t1);
							}
							longAtput(((((char *) t3)) + 4) + (((((t2 >> 1)) + t8) - 1) << 2), t1);
							goto l158;
						}
						if (t7 < 8) {
							t6 = positive32BitValueOf(t1);
							if (successFlag) {
								longAtput(((((char *) t3)) + 4) + ((((t2 >> 1)) - 1) << 2), t6);
							}
							goto l158;
						}
						if (t7 >= 16) {
							t6 = asciiOfCharacter(t1);
							if (!(successFlag)) {
								goto l158;
							}
						} else {
							t6 = t1;
						}
						if ((t6 & 1)) {
							byteAtput(((((char *) t3)) + 4) + (((t2 >> 1)) - 1), (t6 >> 1));
							goto l158;
						}
					}
					successFlag = 0;
				l158:	/* end commonVariable:at:put:cacheIndex: */;
					if (successFlag) {
						currentBytecode = byteAt(++localIP);
						longAtput(localSP -= (3 - 1) * 4, t1);
						goto l157;
					}
				}
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((17 * 2) << 2));
			argumentCount = 2;
			/* begin normalSend */
			goto commonSend;
		l160:	/* end fetchClassOf: */;
		l159:	/* end lookupInMethodCacheSel:class: */;
		l161:	/* end internalExecuteNewMethod */;
		l157:	/* end case */;
			break;
		case 194:
			/* bytecodePrimSize */
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((18 * 2) << 2));
			argumentCount = 0;
			/* begin normalSend */
			goto commonSend;
		l164:	/* end fetchClassOf: */;
		l163:	/* end lookupInMethodCacheSel:class: */;
		l165:	/* end internalExecuteNewMethod */;
			break;
		case 195:
			/* bytecodePrimNext */
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((19 * 2) << 2));
			argumentCount = 0;
			/* begin normalSend */
			goto commonSend;
		l168:	/* end fetchClassOf: */;
		l167:	/* end lookupInMethodCacheSel:class: */;
		l169:	/* end internalExecuteNewMethod */;
			break;
		case 196:
			/* bytecodePrimNextPut */
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((20 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l172:	/* end fetchClassOf: */;
		l171:	/* end lookupInMethodCacheSel:class: */;
		l173:	/* end internalExecuteNewMethod */;
			break;
		case 197:
			/* bytecodePrimAtEnd */
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((21 * 2) << 2));
			argumentCount = 0;
			/* begin normalSend */
			goto commonSend;
		l176:	/* end fetchClassOf: */;
		l175:	/* end lookupInMethodCacheSel:class: */;
		l177:	/* end internalExecuteNewMethod */;
			break;
		case 198:
			/* bytecodePrimEquivalent */
			t2 = longAt(localSP - (1 * 4));
			t1 = longAt(localSP - (0 * 4));
			/* begin booleanCheat: */
			t3 = byteAt(++localIP);
			/* begin internalPop: */
			localSP -= 2 * 4;
			if ((t3 < 160) && (t3 > 151)) {
				if (t2 == t1) {
					currentBytecode = byteAt(++localIP);
					goto l179;
				} else {
					/* begin jump: */
					localIP = (localIP + (t3 - 151)) + 1;
					currentBytecode = byteAt(localIP);
					goto l179;
				}
			}
			if (t3 == 172) {
				t4 = byteAt(++localIP);
				if (t2 == t1) {
					currentBytecode = byteAt(++localIP);
					goto l179;
				} else {
					/* begin jump: */
					localIP = (localIP + t4) + 1;
					currentBytecode = byteAt(localIP);
					goto l179;
				}
			}
			localIP -= 1;
			currentBytecode = byteAt(++localIP);
			if (t2 == t1) {
				longAtput(localSP += 4, trueObj);
			} else {
				longAtput(localSP += 4, falseObj);
			}
		l179:	/* end booleanCheat: */;
			break;
		case 199:
			/* bytecodePrimClass */
			t1 = longAt(localSP);
			/* begin internalPop:thenPush: */
			/* begin fetchClassOf: */
			if ((t1 & 1)) {
				t2 = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
				goto l180;
			}
			t3 = (((unsigned) (longAt(t1))) >> 12) & 31;
			if (t3 == 0) {
				t2 = (longAt(t1 - 4)) & 4294967292U;
				goto l180;
			} else {
				t2 = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((t3 - 1) << 2));
				goto l180;
			}
		l180:	/* end fetchClassOf: */;
			longAtput(localSP -= (1 - 1) * 4, t2);
			currentBytecode = byteAt(++localIP);
			break;
		case 200:
			/* bytecodePrimBlockCopy */
			t2 = longAt(localSP - (1 * 4));
			successFlag = 1;
			t1 = longAt(t2);
			/* begin success: */
			/* begin isContextHeader: */
			t18 = (((unsigned) t1) >> 12) & 31;
			t8 = (t18 == 13) || (t18 == 14);
			successFlag = t8 && successFlag;
			if (successFlag) {
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				primitiveBlockCopy();
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
			}
			if (!(successFlag)) {
				messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((24 * 2) << 2));
				argumentCount = 1;
				/* begin normalSend */
				goto commonSend;
			l183:	/* end fetchClassOf: */;
			l182:	/* end lookupInMethodCacheSel:class: */;
			l185:	/* end internalExecuteNewMethod */;
			}
			currentBytecode = byteAt(++localIP);
		l181:	/* end case */;
			break;
		case 201:
			/* bytecodePrimValue */
			t1 = longAt(localSP);
			successFlag = 1;
			argumentCount = 0;
			if (successFlag) {
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				primitiveValue();
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
			}
			if (!(successFlag)) {
				messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((25 * 2) << 2));
				argumentCount = 0;
				/* begin normalSend */
				goto commonSend;
			l188:	/* end fetchClassOf: */;
			l187:	/* end lookupInMethodCacheSel:class: */;
			l189:	/* end internalExecuteNewMethod */;
			}
			currentBytecode = byteAt(++localIP);
		l186:	/* end case */;
			break;
		case 202:
			/* bytecodePrimValueWithArg */
			t1 = longAt(localSP - (1 * 4));
			successFlag = 1;
			argumentCount = 1;
			if (successFlag) {
				/* begin externalizeIPandSP */
				instructionPointer = ((int) localIP);
				stackPointer = ((int) localSP);
				theHomeContext = localHomeContext;
				primitiveValue();
				/* begin internalizeIPandSP */
				localIP = ((char *) instructionPointer);
				localSP = ((char *) stackPointer);
				localHomeContext = theHomeContext;
			}
			if (!(successFlag)) {
				messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((26 * 2) << 2));
				argumentCount = 1;
				/* begin normalSend */
				goto commonSend;
			l193:	/* end fetchClassOf: */;
			l192:	/* end lookupInMethodCacheSel:class: */;
			l194:	/* end internalExecuteNewMethod */;
			}
			currentBytecode = byteAt(++localIP);
		l191:	/* end case */;
			break;
		case 203:
			/* bytecodePrimDo */
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((27 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l197:	/* end fetchClassOf: */;
		l196:	/* end lookupInMethodCacheSel:class: */;
		l198:	/* end internalExecuteNewMethod */;
			break;
		case 204:
			/* bytecodePrimNew */
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((28 * 2) << 2));
			argumentCount = 0;
			/* begin normalSend */
			goto commonSend;
		l201:	/* end fetchClassOf: */;
		l200:	/* end lookupInMethodCacheSel:class: */;
		l202:	/* end internalExecuteNewMethod */;
			break;
		case 205:
			/* bytecodePrimNewWithArg */
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((29 * 2) << 2));
			argumentCount = 1;
			/* begin normalSend */
			goto commonSend;
		l205:	/* end fetchClassOf: */;
		l204:	/* end lookupInMethodCacheSel:class: */;
		l207:	/* end internalExecuteNewMethod */;
			break;
		case 206:
			/* bytecodePrimPointX */
			successFlag = 1;
			/* begin externalizeIPandSP */
			instructionPointer = ((int) localIP);
			stackPointer = ((int) localSP);
			theHomeContext = localHomeContext;
			primitivePointX();
			/* begin internalizeIPandSP */
			localIP = ((char *) instructionPointer);
			localSP = ((char *) stackPointer);
			localHomeContext = theHomeContext;
			if (successFlag) {
				currentBytecode = byteAt(++localIP);
				goto l208;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((30 * 2) << 2));
			argumentCount = 0;
			/* begin normalSend */
			goto commonSend;
		l210:	/* end fetchClassOf: */;
		l209:	/* end lookupInMethodCacheSel:class: */;
		l212:	/* end internalExecuteNewMethod */;
		l208:	/* end case */;
			break;
		case 207:
			/* bytecodePrimPointY */
			successFlag = 1;
			/* begin externalizeIPandSP */
			instructionPointer = ((int) localIP);
			stackPointer = ((int) localSP);
			theHomeContext = localHomeContext;
			primitivePointY();
			/* begin internalizeIPandSP */
			localIP = ((char *) instructionPointer);
			localSP = ((char *) stackPointer);
			localHomeContext = theHomeContext;
			if (successFlag) {
				currentBytecode = byteAt(++localIP);
				goto l213;
			}
			messageSelector = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((31 * 2) << 2));
			argumentCount = 0;
			/* begin normalSend */
			goto commonSend;
		l215:	/* end fetchClassOf: */;
		l214:	/* end lookupInMethodCacheSel:class: */;
		l216:	/* end internalExecuteNewMethod */;
		l213:	/* end case */;
			break;
		case 208:
		case 209:
		case 210:
		case 211:
		case 212:
		case 213:
		case 214:
		case 215:
		case 216:
		case 217:
		case 218:
		case 219:
		case 220:
		case 221:
		case 222:
		case 223:
		case 224:
		case 225:
		case 226:
		case 227:
		case 228:
		case 229:
		case 230:
		case 231:
		case 232:
		case 233:
		case 234:
		case 235:
		case 236:
		case 237:
		case 238:
		case 239:
		case 240:
		case 241:
		case 242:
		case 243:
		case 244:
		case 245:
		case 246:
		case 247:
		case 248:
		case 249:
		case 250:
		case 251:
		case 252:
		case 253:
		case 254:
		case 255:
			/* sendLiteralSelectorBytecode */
			/* begin literal: */
			t1 = currentBytecode & 15;
			messageSelector = longAt(((((char *) method)) + 4) + ((t1 + 1) << 2));
			argumentCount = ((((unsigned) currentBytecode) >> 4) & 3) - 1;
			/* begin normalSend */
			goto commonSend;
		l219:	/* end fetchClassOf: */;
		l218:	/* end lookupInMethodCacheSel:class: */;
		l220:	/* end internalExecuteNewMethod */;
			break;
		}
	}
	/* begin externalizeIPandSP */
	instructionPointer = ((int) localIP);
	stackPointer = ((int) localSP);
	theHomeContext = localHomeContext;
}

int isBytes(int oop) {
	return ((((unsigned) (longAt(oop))) >> 8) & 15) >= 8;
}

int isContextHeader(int aHeader) {
    int ccIndex;

	ccIndex = (((unsigned) aHeader) >> 12) & 31;
	return (ccIndex == 13) || (ccIndex == 14);
}

int isEmptyList(int aLinkedList) {
	return (longAt(((((char *) aLinkedList)) + 4) + (0 << 2))) == nilObj;
}

int isFreeObject(int oop) {
	return ((longAt(oop)) & 3) == 2;
}

int isIntegerObject(int objectPointer) {
	return (objectPointer & 1) > 0;
}

int isIntegerValue(int intValue) {
	return (intValue ^ (intValue << 1)) >= 0;
}

int isMethodContextHeader(int aHeader) {
	return ((((unsigned) aHeader) >> 12) & 31) == 14;
}

int isObjectForwarded(int oop) {
	return ((oop & 1) == 0) && (((longAt(oop)) & 2147483648U) != 0);
}

int isPointers(int oop) {
	return ((((unsigned) (longAt(oop))) >> 8) & 15) <= 4;
}

int isWeak(int oop) {
	return ((((unsigned) (longAt(oop))) >> 8) & 15) == 4;
}

int isWords(int oop) {
	return ((((unsigned) (longAt(oop))) >> 8) & 15) == 6;
}

int isWordsOrBytes(int oop) {
    int fmt;

	fmt = (((unsigned) (longAt(oop))) >> 8) & 15;
	return (fmt == 6) || ((fmt >= 8) && (fmt <= 11));
}

int lastPointerOf(int oop) {
    int methodHeader;
    int sz;
    int fmt;
    int header;
    int header1;
    int type;

	header = longAt(oop);
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			return (6 + (fetchStackPointerOf(oop))) * 4;
		}
		/* begin sizeBitsOfSafe: */
		header1 = longAt(oop);
		/* begin rightType: */
		if ((header1 & 252) == 0) {
			type = 0;
			goto l1;
		} else {
			if ((header1 & 126976) == 0) {
				type = 1;
				goto l1;
			} else {
				type = 3;
				goto l1;
			}
		}
	l1:	/* end rightType: */;
		if (type == 0) {
			sz = (longAt(oop - 8)) & 4294967292U;
			goto l2;
		} else {
			sz = header1 & 252;
			goto l2;
		}
	l2:	/* end sizeBitsOfSafe: */;
		return sz - 4;
	}
	if (fmt < 12) {
		return 0;
	}
	methodHeader = longAt(oop + 4);
	return (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
}

int lastPointerWhileForwarding(int oop) {
    int methodHeader;
    int size;
    int fwdBlock;
    int fmt;
    int header;

	header = longAt(oop);
	if ((header & 2147483648U) != 0) {
		fwdBlock = (header & 2147483644) << 1;
		;
		header = longAt(fwdBlock + 4);
	}
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			return (6 + (fetchStackPointerOf(oop))) * 4;
		}
		if ((header & 3) == 0) {
			size = (longAt(oop - 8)) & 4294967292U;
		} else {
			size = header & 252;
		}
		return size - 4;
	}
	if (fmt < 12) {
		return 0;
	}
	methodHeader = longAt(oop + 4);
	return (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
}

int lengthOf(int oop) {
    int header;
    int sz;

	header = longAt(oop);
	/* begin lengthOf:baseHeader:format: */
	if ((header & 3) == 0) {
		sz = (longAt(oop - 8)) & 4294967292U;
	} else {
		sz = header & 252;
	}
	if (((((unsigned) header) >> 8) & 15) < 8) {
		return ((unsigned) (sz - 4)) >> 2;
	} else {
		return (sz - 4) - (((((unsigned) header) >> 8) & 15) & 3);
	}
	return null;
}

int lengthOfbaseHeaderformat(int oop, int hdr, int fmt) {
    int sz;

	if ((hdr & 3) == 0) {
		sz = (longAt(oop - 8)) & 4294967292U;
	} else {
		sz = hdr & 252;
	}
	if (fmt < 8) {
		return ((unsigned) (sz - 4)) >> 2;
	} else {
		return (sz - 4) - (fmt & 3);
	}
}

int literal(int offset) {
	return longAt(((((char *) method)) + 4) + ((offset + 1) << 2));
}

int literalofMethod(int offset, int methodPointer) {
	return longAt(((((char *) methodPointer)) + 4) + ((offset + 1) << 2));
}

int literalCountOf(int methodPointer) {
	return (((unsigned) (longAt(((((char *) methodPointer)) + 4) + (0 << 2)))) >> 10) & 255;
}

int literalCountOfHeader(int headerPointer) {
	return (((unsigned) headerPointer) >> 10) & 255;
}

int loadBitBltFrom(int bbObj) {
    int destBitsSize;
    int destWidth;
    int destHeight;
    int sourceBitsSize;
    int sourcePixPerWord;
    int halftoneBits;
    int cmSize;
    int header;
    int sz;
    int header1;
    int sz1;

	bitBltOop = bbObj;
	combinationRule = fetchIntegerofObject(3, bitBltOop);
	if ((!successFlag) || ((combinationRule < 0) || (combinationRule > (36 - 2)))) {
		return 0;
	}
	if ((combinationRule >= 16) && (combinationRule <= 17)) {
		return 0;
	}
	sourceForm = longAt(((((char *) bitBltOop)) + 4) + (1 << 2));
	/* begin ignoreSourceOrHalftone: */
	if (sourceForm == nilObj) {
		noSource = 1;
		goto l3;
	}
	if (combinationRule == 0) {
		noSource = 1;
		goto l3;
	}
	if (combinationRule == 5) {
		noSource = 1;
		goto l3;
	}
	if (combinationRule == 10) {
		noSource = 1;
		goto l3;
	}
	if (combinationRule == 15) {
		noSource = 1;
		goto l3;
	}
	noSource = 0;
l3:	/* end ignoreSourceOrHalftone: */;
	halftoneForm = longAt(((((char *) bitBltOop)) + 4) + (2 << 2));
	/* begin ignoreSourceOrHalftone: */
	if (halftoneForm == nilObj) {
		noHalftone = 1;
		goto l4;
	}
	if (combinationRule == 0) {
		noHalftone = 1;
		goto l4;
	}
	if (combinationRule == 5) {
		noHalftone = 1;
		goto l4;
	}
	if (combinationRule == 10) {
		noHalftone = 1;
		goto l4;
	}
	if (combinationRule == 15) {
		noHalftone = 1;
		goto l4;
	}
	noHalftone = 0;
l4:	/* end ignoreSourceOrHalftone: */;
	destForm = longAt(((((char *) bitBltOop)) + 4) + (0 << 2));
	if (!((((((unsigned) (longAt(destForm))) >> 8) & 15) <= 4) && ((lengthOf(destForm)) >= 4))) {
		return 0;
	}
	destBits = longAt(((((char *) destForm)) + 4) + (0 << 2));
	destBitsSize = byteLengthOf(destBits);
	destWidth = fetchIntegerofObject(1, destForm);
	destHeight = fetchIntegerofObject(2, destForm);
	if (!((destWidth >= 0) && (destHeight >= 0))) {
		return 0;
	}
	destPixSize = fetchIntegerofObject(3, destForm);
	pixPerWord = 32 / destPixSize;
	destRaster = (destWidth + (pixPerWord - 1)) / pixPerWord;
	if (!((isWordsOrBytes(destBits)) && (destBitsSize == ((destRaster * destHeight) * 4)))) {
		return 0;
	}
	destX = fetchIntegerOrTruncFloatofObject(4, bitBltOop);
	destY = fetchIntegerOrTruncFloatofObject(5, bitBltOop);
	width = fetchIntegerOrTruncFloatofObject(6, bitBltOop);
	height = fetchIntegerOrTruncFloatofObject(7, bitBltOop);
	if (!successFlag) {
		return 0;
	}
	if (noSource) {
		sourceX = sourceY = 0;
	} else {
		if (!((((((unsigned) (longAt(sourceForm))) >> 8) & 15) <= 4) && ((lengthOf(sourceForm)) >= 4))) {
			return 0;
		}
		sourceBits = longAt(((((char *) sourceForm)) + 4) + (0 << 2));
		sourceBitsSize = byteLengthOf(sourceBits);
		srcWidth = fetchIntegerOrTruncFloatofObject(1, sourceForm);
		srcHeight = fetchIntegerOrTruncFloatofObject(2, sourceForm);
		if (!((srcWidth >= 0) && (srcHeight >= 0))) {
			return 0;
		}
		sourcePixSize = fetchIntegerofObject(3, sourceForm);
		sourcePixPerWord = 32 / sourcePixSize;
		sourceRaster = (srcWidth + (sourcePixPerWord - 1)) / sourcePixPerWord;
		if (!((isWordsOrBytes(sourceBits)) && (sourceBitsSize == ((sourceRaster * srcHeight) * 4)))) {
			return 0;
		}
		colorMap = longAt(((((char *) bitBltOop)) + 4) + (14 << 2));
		if (!(colorMap == nilObj)) {
			if (((((unsigned) (longAt(colorMap))) >> 8) & 15) == 6) {
				/* begin lengthOf: */
				header = longAt(colorMap);
				/* begin lengthOf:baseHeader:format: */
				if ((header & 3) == 0) {
					sz = (longAt(colorMap - 8)) & 4294967292U;
				} else {
					sz = header & 252;
				}
				if (((((unsigned) header) >> 8) & 15) < 8) {
					cmSize = ((unsigned) (sz - 4)) >> 2;
					goto l1;
				} else {
					cmSize = (sz - 4) - (((((unsigned) header) >> 8) & 15) & 3);
					goto l1;
				}
				cmSize = null;
			l1:	/* end lengthOf: */;
				cmBitsPerColor = 0;
				if (cmSize == 512) {
					cmBitsPerColor = 3;
				}
				if (cmSize == 4096) {
					cmBitsPerColor = 4;
				}
				if (cmSize == 32768) {
					cmBitsPerColor = 5;
				}
				if (primitiveIndex != 147) {
					if (sourcePixSize <= 8) {
						if (!(cmSize == (1 << sourcePixSize))) {
							return 0;
						}
					} else {
						if (cmBitsPerColor == 0) {
							return 0;
						}
					}
				}
			} else {
				return 0;
			}
		}
		sourceX = fetchIntegerOrTruncFloatofObject(8, bitBltOop);
		sourceY = fetchIntegerOrTruncFloatofObject(9, bitBltOop);
	}
	if (!(noHalftone)) {
		if ((((((unsigned) (longAt(halftoneForm))) >> 8) & 15) <= 4) && ((lengthOf(halftoneForm)) >= 4)) {
			halftoneBits = longAt(((((char *) halftoneForm)) + 4) + (0 << 2));
			halftoneHeight = fetchIntegerofObject(2, halftoneForm);
			if (!(((((unsigned) (longAt(halftoneBits))) >> 8) & 15) == 6)) {
				noHalftone = 1;
			}
		} else {
			if (!((!(((((unsigned) (longAt(halftoneForm))) >> 8) & 15) <= 4)) && (((((unsigned) (longAt(halftoneForm))) >> 8) & 15) == 6))) {
				return 0;
			}
			halftoneBits = halftoneForm;
			/* begin lengthOf: */
			header1 = longAt(halftoneBits);
			/* begin lengthOf:baseHeader:format: */
			if ((header1 & 3) == 0) {
				sz1 = (longAt(halftoneBits - 8)) & 4294967292U;
			} else {
				sz1 = header1 & 252;
			}
			if (((((unsigned) header1) >> 8) & 15) < 8) {
				halftoneHeight = ((unsigned) (sz1 - 4)) >> 2;
				goto l2;
			} else {
				halftoneHeight = (sz1 - 4) - (((((unsigned) header1) >> 8) & 15) & 3);
				goto l2;
			}
			halftoneHeight = null;
		l2:	/* end lengthOf: */;
		}
		halftoneBase = halftoneBits + 4;
	}
	clipX = fetchIntegerOrTruncFloatofObject(10, bitBltOop);
	clipY = fetchIntegerOrTruncFloatofObject(11, bitBltOop);
	clipWidth = fetchIntegerOrTruncFloatofObject(12, bitBltOop);
	clipHeight = fetchIntegerOrTruncFloatofObject(13, bitBltOop);
	if (!successFlag) {
		return 0;
	}
	if (clipX < 0) {
		clipWidth += clipX;
		clipX = 0;
	}
	if (clipY < 0) {
		clipHeight += clipY;
		clipY = 0;
	}
	if ((clipX + clipWidth) > destWidth) {
		clipWidth = destWidth - clipX;
	}
	if ((clipY + clipHeight) > destHeight) {
		clipHeight = destHeight - clipY;
	}
	return 1;
}

double loadFloatOrIntFrom(int floatOrInt) {
	if ((floatOrInt & 1)) {
		return ((double) (floatOrInt >> 1));
	}
	if ((fetchClassOfNonInt(floatOrInt)) == (longAt(((((char *) specialObjectsOop)) + 4) + (9 << 2)))) {
		return floatValueOf(floatOrInt);
	}
	successFlag = 0;
}

void loadInitialContext(void) {
    int sched;
    int proc;
    int activeCntx;
    int tmp;

	sched = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2));
	proc = longAt(((((char *) sched)) + 4) + (1 << 2));
	activeContext = longAt(((((char *) proc)) + 4) + (1 << 2));
	if (activeContext < youngStart) {
		beRootIfOld(activeContext);
	}
	/* begin fetchContextRegisters: */
	activeCntx = activeContext;
	tmp = longAt(((((char *) activeCntx)) + 4) + (3 << 2));
	if ((tmp & 1)) {
		tmp = longAt(((((char *) activeCntx)) + 4) + (5 << 2));
		if (tmp < youngStart) {
			beRootIfOld(tmp);
		}
	} else {
		tmp = activeCntx;
	}
	theHomeContext = tmp;
	receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
	method = longAt(((((char *) tmp)) + 4) + (3 << 2));
	tmp = ((longAt(((((char *) activeCntx)) + 4) + (1 << 2))) >> 1);
	instructionPointer = ((method + tmp) + 4) - 2;
	tmp = ((longAt(((((char *) activeCntx)) + 4) + (2 << 2))) >> 1);
	stackPointer = (activeCntx + 4) + (((6 + tmp) - 1) * 4);
	reclaimableContextCount = 0;
}

int loadScannerFromstartstopstringrightXstopArraydisplayFlag(int bbObj, int start, int stop, int string, int rightX, int stopArray, int displayFlag) {
    int successValue;
    int successValue1;
    int successValue2;

	scanStart = start;
	scanStop = stop;
	scanString = string;
	scanRightX = rightX;
	scanStopArray = stopArray;
	scanDisplayFlag = displayFlag;
	/* begin success: */
	successValue1 = (((((unsigned) (longAt(scanStopArray))) >> 8) & 15) <= 4) && ((lengthOf(scanStopArray)) >= 1);
	successFlag = successValue1 && successFlag;
	scanXTable = longAt(((((char *) bbObj)) + 4) + (16 << 2));
	/* begin success: */
	successValue2 = (((((unsigned) (longAt(scanXTable))) >> 8) & 15) <= 4) && ((lengthOf(scanXTable)) >= 1);
	successFlag = successValue2 && successFlag;
	/* begin storeInteger:ofObject:withValue: */
	if ((0 ^ (0 << 1)) >= 0) {
		longAtput(((((char *) bbObj)) + 4) + (6 << 2), ((0 << 1) | 1));
	} else {
		successFlag = 0;
	}
	/* begin storeInteger:ofObject:withValue: */
	if ((0 ^ (0 << 1)) >= 0) {
		longAtput(((((char *) bbObj)) + 4) + (8 << 2), ((0 << 1) | 1));
	} else {
		successFlag = 0;
	}
	if (scanDisplayFlag) {
		/* begin success: */
		successValue = loadBitBltFrom(bbObj);
		successFlag = successValue && successFlag;
	} else {
		bitBltOop = bbObj;
		destX = fetchIntegerOrTruncFloatofObject(4, bbObj);
	}
	return !(!successFlag);
}

int lookupInMethodCacheSelclass(int selector, int cls) {
    int probe;
    int hash;

	hash = selector ^ cls;
	probe = hash & 1020;
	if (((methodCache[probe + 1]) == selector) && ((methodCache[probe + 2]) == cls)) {
		newMethod = methodCache[probe + 3];
		primitiveIndex = methodCache[probe + 4];
		return 1;
	}
	probe = (((unsigned) hash) >> 1) & 1020;
	if (((methodCache[probe + 1]) == selector) && ((methodCache[probe + 2]) == cls)) {
		newMethod = methodCache[probe + 3];
		primitiveIndex = methodCache[probe + 4];
		return 1;
	}
	probe = (((unsigned) hash) >> 2) & 1020;
	if (((methodCache[probe + 1]) == selector) && ((methodCache[probe + 2]) == cls)) {
		newMethod = methodCache[probe + 3];
		primitiveIndex = methodCache[probe + 4];
		return 1;
	}
	return 0;
}

int lookupMethodInClass(int cls) {
    int dictionary;
    int currentClass;
    int found;
    int rclass;
    int oop;
    int argumentArray;
    int message;
    int oop1;
    int valuePointer;
    int lastIn;
    int in;
    int out;
    int sp;
    int methodArray;
    int mask;
    int wrapAround;
    int nextSelector;
    int index;
    int length;
    int sz;
    int primBits;
    int header;

	currentClass = cls;
	while (currentClass != nilObj) {
		dictionary = longAt(((((char *) currentClass)) + 4) + (1 << 2));
		/* begin lookupMethodInDictionary: */
		/* begin fetchWordLengthOf: */
		/* begin sizeBitsOf: */
		header = longAt(dictionary);
		if ((header & 3) == 0) {
			sz = (longAt(dictionary - 8)) & 4294967292U;
			goto l2;
		} else {
			sz = header & 252;
			goto l2;
		}
	l2:	/* end sizeBitsOf: */;
		length = ((unsigned) (sz - 4)) >> 2;
		mask = (length - 2) - 1;
		if ((messageSelector & 1)) {
			index = (mask & ((messageSelector >> 1))) + 2;
		} else {
			index = (mask & ((((unsigned) (longAt(messageSelector))) >> 17) & 4095)) + 2;
		}
		wrapAround = 0;
		while (1) {
			nextSelector = longAt(((((char *) dictionary)) + 4) + (index << 2));
			if (nextSelector == nilObj) {
				found = 0;
				goto l3;
			}
			if (nextSelector == messageSelector) {
				methodArray = longAt(((((char *) dictionary)) + 4) + (1 << 2));
				newMethod = longAt(((((char *) methodArray)) + 4) + ((index - 2) << 2));
				/* begin primitiveIndexOf: */
				primBits = (((unsigned) (longAt(((((char *) newMethod)) + 4) + (0 << 2)))) >> 1) & 805306879;
				if (primBits > 511) {
					primitiveIndex = (primBits & 511) + (((unsigned) primBits) >> 19);
					goto l1;
				} else {
					primitiveIndex = primBits;
					goto l1;
				}
			l1:	/* end primitiveIndexOf: */;
				if (primitiveIndex > 520) {
					primitiveIndex = 0;
				}
				found = 1;
				goto l3;
			}
			index += 1;
			if (index == length) {
				if (wrapAround) {
					found = 0;
					goto l3;
				}
				wrapAround = 1;
				index = 2;
			}
		}
	l3:	/* end lookupMethodInDictionary: */;
		if (found) {
			return currentClass;
		}
		currentClass = longAt(((((char *) currentClass)) + 4) + (0 << 2));
	}
	if (messageSelector == (longAt(((((char *) specialObjectsOop)) + 4) + (20 << 2)))) {
		error("Recursive not understood error encountered");
	}
	/* begin pushRemappableOop: */
	remapBuffer[remapBufferCount += 1] = cls;
	/* begin createActualMessage */
	argumentArray = instantiateClassindexableSize(longAt(((((char *) specialObjectsOop)) + 4) + (7 << 2)), argumentCount);
	/* begin pushRemappableOop: */
	remapBuffer[remapBufferCount += 1] = argumentArray;
	message = instantiateClassindexableSize(longAt(((((char *) specialObjectsOop)) + 4) + (15 << 2)), 0);
	/* begin popRemappableOop */
	oop1 = remapBuffer[remapBufferCount];
	remapBufferCount -= 1;
	argumentArray = oop1;
	if (argumentArray < youngStart) {
		beRootIfOld(argumentArray);
	}
	/* begin storePointer:ofObject:withValue: */
	valuePointer = messageSelector;
	if (message < youngStart) {
		possibleRootStoreIntovalue(message, valuePointer);
	}
	longAtput(((((char *) message)) + 4) + (0 << 2), valuePointer);
	/* begin storePointer:ofObject:withValue: */
	if (message < youngStart) {
		possibleRootStoreIntovalue(message, argumentArray);
	}
	longAtput(((((char *) message)) + 4) + (1 << 2), argumentArray);
	/* begin transfer:from:to: */
	in = (stackPointer - ((argumentCount - 1) * 4)) - 4;
	lastIn = in + (argumentCount * 4);
	out = (argumentArray + 4) - 4;
	while (in < lastIn) {
		longAtput(out += 4, longAt(in += 4));
	}
	/* begin pop: */
	stackPointer -= argumentCount * 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, message);
	stackPointer = sp;
	argumentCount = 1;
	/* begin popRemappableOop */
	oop = remapBuffer[remapBufferCount];
	remapBufferCount -= 1;
	rclass = oop;
	messageSelector = longAt(((((char *) specialObjectsOop)) + 4) + (20 << 2));
	return lookupMethodInClass(rclass);
}

int lookupMethodInDictionary(int dictionary) {
    int methodArray;
    int mask;
    int wrapAround;
    int nextSelector;
    int index;
    int length;
    int sz;
    int primBits;
    int header;

	/* begin fetchWordLengthOf: */
	/* begin sizeBitsOf: */
	header = longAt(dictionary);
	if ((header & 3) == 0) {
		sz = (longAt(dictionary - 8)) & 4294967292U;
		goto l2;
	} else {
		sz = header & 252;
		goto l2;
	}
l2:	/* end sizeBitsOf: */;
	length = ((unsigned) (sz - 4)) >> 2;
	mask = (length - 2) - 1;
	if ((messageSelector & 1)) {
		index = (mask & ((messageSelector >> 1))) + 2;
	} else {
		index = (mask & ((((unsigned) (longAt(messageSelector))) >> 17) & 4095)) + 2;
	}
	wrapAround = 0;
	while (1) {
		nextSelector = longAt(((((char *) dictionary)) + 4) + (index << 2));
		if (nextSelector == nilObj) {
			return 0;
		}
		if (nextSelector == messageSelector) {
			methodArray = longAt(((((char *) dictionary)) + 4) + (1 << 2));
			newMethod = longAt(((((char *) methodArray)) + 4) + ((index - 2) << 2));
			/* begin primitiveIndexOf: */
			primBits = (((unsigned) (longAt(((((char *) newMethod)) + 4) + (0 << 2)))) >> 1) & 805306879;
			if (primBits > 511) {
				primitiveIndex = (primBits & 511) + (((unsigned) primBits) >> 19);
				goto l1;
			} else {
				primitiveIndex = primBits;
				goto l1;
			}
		l1:	/* end primitiveIndexOf: */;
			if (primitiveIndex > 520) {
				primitiveIndex = 0;
			}
			return 1;
		}
		index += 1;
		if (index == length) {
			if (wrapAround) {
				return 0;
			}
			wrapAround = 1;
			index = 2;
		}
	}
}

int lowestFreeAfter(int chunk) {
    int oopHeader;
    int oop;
    int oopHeaderType;
    int oopSize;
    int extra;
    int extra1;
    int type;
    int extra2;
    int type1;
    int extra3;

	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type1 = (longAt(chunk)) & 3;
	if (type1 > 1) {
		extra3 = 0;
	} else {
		if (type1 == 1) {
			extra3 = 4;
		} else {
			extra3 = 8;
		}
	}
	extra1 = extra3;
	oop = chunk + extra1;
	while (oop < endOfMemory) {
		oopHeader = longAt(oop);
		oopHeaderType = oopHeader & 3;
		if (oopHeaderType == 2) {
			return oop;
		} else {
			if (oopHeaderType == 0) {
				oopSize = (longAt(oop - 8)) & 4294967292U;
			} else {
				oopSize = oopHeader & 252;
			}
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type = (longAt(oop + oopSize)) & 3;
		if (type > 1) {
			extra2 = 0;
		} else {
			if (type == 1) {
				extra2 = 4;
			} else {
				extra2 = 8;
			}
		}
		extra = extra2;
		oop = (oop + oopSize) + extra;
	}
	error("expected to find at least one free object");
}

int makePointwithxValueyValue(int xValue, int yValue) {
    int pointResult;

	pointResult = instantiateSmallClasssizeInBytesfill(longAt(((((char *) specialObjectsOop)) + 4) + (12 << 2)), 12, nilObj);
	/* begin storePointer:ofObject:withValue: */
	if (pointResult < youngStart) {
		possibleRootStoreIntovalue(pointResult, ((xValue << 1) | 1));
	}
	longAtput(((((char *) pointResult)) + 4) + (0 << 2), ((xValue << 1) | 1));
	/* begin storePointer:ofObject:withValue: */
	if (pointResult < youngStart) {
		possibleRootStoreIntovalue(pointResult, ((yValue << 1) | 1));
	}
	longAtput(((((char *) pointResult)) + 4) + (1 << 2), ((yValue << 1) | 1));
	return pointResult;
}

void mapInterpreterOops(void) {
    int i;
    int oop;
    int i1;

	nilObj = remap(nilObj);
	falseObj = remap(falseObj);
	trueObj = remap(trueObj);
	specialObjectsOop = remap(specialObjectsOop);
	stackPointer -= activeContext;
	activeContext = remap(activeContext);
	stackPointer += activeContext;
	theHomeContext = remap(theHomeContext);
	instructionPointer -= method;
	method = remap(method);
	instructionPointer += method;
	receiver = remap(receiver);
	messageSelector = remap(messageSelector);
	newMethod = remap(newMethod);
	for (i = 1; i <= remapBufferCount; i += 1) {
		oop = remapBuffer[i];
		if (!((oop & 1))) {
			remapBuffer[i] = (remap(oop));
		}
	}
	/* begin flushMethodCache */
	for (i1 = 1; i1 <= 1024; i1 += 1) {
		methodCache[i1] = 0;
	}
	for (i1 = 1; i1 <= 64; i1 += 1) {
		atCache[i1] = 0;
	}
}

void mapPointersInObjectsFromto(int memStart, int memEnd) {
    int oop;
    int i;
    int fwdBlock;
    int fieldOffset;
    int fieldOop;
    int newOop;
    int fwdBlock1;
    int fieldOffset1;
    int fieldOop1;
    int newOop1;
    int i2;
    int oop1;
    int i1;
    int extra;
    int type;
    int extra1;
    int methodHeader;
    int size;
    int fwdBlock2;
    int fmt;
    int header;
    int newClassOop;
    int fwdBlock3;
    int classHeader;
    int classOop;
    int newClassHeader;
    int methodHeader1;
    int size1;
    int fwdBlock4;
    int fmt1;
    int header1;
    int newClassOop1;
    int fwdBlock5;
    int classHeader1;
    int classOop1;
    int newClassHeader1;
    int sz;
    int fwdBlock6;
    int realHeader;
    int header2;
    int extra3;
    int type2;
    int extra12;
    int sz1;
    int header11;
    int extra2;
    int type1;
    int extra11;

	/* begin mapInterpreterOops */
	nilObj = remap(nilObj);
	falseObj = remap(falseObj);
	trueObj = remap(trueObj);
	specialObjectsOop = remap(specialObjectsOop);
	stackPointer -= activeContext;
	activeContext = remap(activeContext);
	stackPointer += activeContext;
	theHomeContext = remap(theHomeContext);
	instructionPointer -= method;
	method = remap(method);
	instructionPointer += method;
	receiver = remap(receiver);
	messageSelector = remap(messageSelector);
	newMethod = remap(newMethod);
	for (i2 = 1; i2 <= remapBufferCount; i2 += 1) {
		oop1 = remapBuffer[i2];
		if (!((oop1 & 1))) {
			remapBuffer[i2] = (remap(oop1));
		}
	}
	/* begin flushMethodCache */
	for (i1 = 1; i1 <= 1024; i1 += 1) {
		methodCache[i1] = 0;
	}
	for (i1 = 1; i1 <= 64; i1 += 1) {
		atCache[i1] = 0;
	}
	for (i = 1; i <= rootTableCount; i += 1) {
		oop = rootTable[i];
		if ((oop < memStart) || (oop >= memEnd)) {
			/* begin remapFieldsAndClassOf: */
			/* begin lastPointerWhileForwarding: */
			header = longAt(oop);
			if ((header & 2147483648U) != 0) {
				fwdBlock2 = (header & 2147483644) << 1;
				;
				header = longAt(fwdBlock2 + 4);
			}
			fmt = (((unsigned) header) >> 8) & 15;
			if (fmt <= 4) {
				if ((fmt == 3) && (isContextHeader(header))) {
					fieldOffset = (6 + (fetchStackPointerOf(oop))) * 4;
					goto l1;
				}
				if ((header & 3) == 0) {
					size = (longAt(oop - 8)) & 4294967292U;
				} else {
					size = header & 252;
				}
				fieldOffset = size - 4;
				goto l1;
			}
			if (fmt < 12) {
				fieldOffset = 0;
				goto l1;
			}
			methodHeader = longAt(oop + 4);
			fieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
		l1:	/* end lastPointerWhileForwarding: */;
			while (fieldOffset >= 4) {
				fieldOop = longAt(oop + fieldOffset);
				if (((fieldOop & 1) == 0) && (((longAt(fieldOop)) & 2147483648U) != 0)) {
					fwdBlock = ((longAt(fieldOop)) & 2147483644) << 1;
					;
					newOop = longAt(fwdBlock);
					longAtput(oop + fieldOffset, newOop);
					if ((oop < youngStart) && (newOop >= youngStart)) {
						beRootWhileForwarding(oop);
					}
				}
				fieldOffset -= 4;
			}
			/* begin remapClassOf: */
			if (((longAt(oop)) & 3) == 3) {
				goto l2;
			}
			classHeader = longAt(oop - 4);
			classOop = classHeader & 4294967292U;
			if (((classOop & 1) == 0) && (((longAt(classOop)) & 2147483648U) != 0)) {
				fwdBlock3 = ((longAt(classOop)) & 2147483644) << 1;
				;
				newClassOop = longAt(fwdBlock3);
				newClassHeader = newClassOop | (classHeader & 3);
				longAtput(oop - 4, newClassHeader);
				if ((oop < youngStart) && (newClassOop >= youngStart)) {
					beRootWhileForwarding(oop);
				}
			}
		l2:	/* end remapClassOf: */;
		}
	}
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type = (longAt(memStart)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	oop = memStart + extra;
	while (oop < memEnd) {
		if (!(((longAt(oop)) & 3) == 2)) {
			/* begin remapFieldsAndClassOf: */
			/* begin lastPointerWhileForwarding: */
			header1 = longAt(oop);
			if ((header1 & 2147483648U) != 0) {
				fwdBlock4 = (header1 & 2147483644) << 1;
				;
				header1 = longAt(fwdBlock4 + 4);
			}
			fmt1 = (((unsigned) header1) >> 8) & 15;
			if (fmt1 <= 4) {
				if ((fmt1 == 3) && (isContextHeader(header1))) {
					fieldOffset1 = (6 + (fetchStackPointerOf(oop))) * 4;
					goto l3;
				}
				if ((header1 & 3) == 0) {
					size1 = (longAt(oop - 8)) & 4294967292U;
				} else {
					size1 = header1 & 252;
				}
				fieldOffset1 = size1 - 4;
				goto l3;
			}
			if (fmt1 < 12) {
				fieldOffset1 = 0;
				goto l3;
			}
			methodHeader1 = longAt(oop + 4);
			fieldOffset1 = (((((unsigned) methodHeader1) >> 10) & 255) * 4) + 4;
		l3:	/* end lastPointerWhileForwarding: */;
			while (fieldOffset1 >= 4) {
				fieldOop1 = longAt(oop + fieldOffset1);
				if (((fieldOop1 & 1) == 0) && (((longAt(fieldOop1)) & 2147483648U) != 0)) {
					fwdBlock1 = ((longAt(fieldOop1)) & 2147483644) << 1;
					;
					newOop1 = longAt(fwdBlock1);
					longAtput(oop + fieldOffset1, newOop1);
					if ((oop < youngStart) && (newOop1 >= youngStart)) {
						beRootWhileForwarding(oop);
					}
				}
				fieldOffset1 -= 4;
			}
			/* begin remapClassOf: */
			if (((longAt(oop)) & 3) == 3) {
				goto l4;
			}
			classHeader1 = longAt(oop - 4);
			classOop1 = classHeader1 & 4294967292U;
			if (((classOop1 & 1) == 0) && (((longAt(classOop1)) & 2147483648U) != 0)) {
				fwdBlock5 = ((longAt(classOop1)) & 2147483644) << 1;
				;
				newClassOop1 = longAt(fwdBlock5);
				newClassHeader1 = newClassOop1 | (classHeader1 & 3);
				longAtput(oop - 4, newClassHeader1);
				if ((oop < youngStart) && (newClassOop1 >= youngStart)) {
					beRootWhileForwarding(oop);
				}
			}
		l4:	/* end remapClassOf: */;
		}
		/* begin objectAfterWhileForwarding: */
		header2 = longAt(oop);
		if ((header2 & 2147483648U) == 0) {
			/* begin objectAfter: */
			;
			if (((longAt(oop)) & 3) == 2) {
				sz1 = (longAt(oop)) & 4294967292U;
			} else {
				/* begin sizeBitsOf: */
				header11 = longAt(oop);
				if ((header11 & 3) == 0) {
					sz1 = (longAt(oop - 8)) & 4294967292U;
					goto l5;
				} else {
					sz1 = header11 & 252;
					goto l5;
				}
			l5:	/* end sizeBitsOf: */;
			}
			/* begin oopFromChunk: */
			/* begin extraHeaderBytes: */
			type1 = (longAt(oop + sz1)) & 3;
			if (type1 > 1) {
				extra11 = 0;
			} else {
				if (type1 == 1) {
					extra11 = 4;
				} else {
					extra11 = 8;
				}
			}
			extra2 = extra11;
			oop = (oop + sz1) + extra2;
			goto l6;
		}
		fwdBlock6 = (header2 & 2147483644) << 1;
		;
		realHeader = longAt(fwdBlock6 + 4);
		if ((realHeader & 3) == 0) {
			sz = (longAt(oop - 8)) & 4294967292U;
		} else {
			sz = realHeader & 252;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type2 = (longAt(oop + sz)) & 3;
		if (type2 > 1) {
			extra12 = 0;
		} else {
			if (type2 == 1) {
				extra12 = 4;
			} else {
				extra12 = 8;
			}
		}
		extra3 = extra12;
		oop = (oop + sz) + extra3;
	l6:	/* end objectAfterWhileForwarding: */;
	}
}

void markAndTrace(int oop) {
    int action;
    int lastFieldOffset;
    int header;
    int typeBits;
    int childType;
    int header1;
    int type;
    int methodHeader;
    int sz;
    int fmt;
    int header2;
    int header3;
    int type1;
    int oop1;
    int lastFieldOffset1;
    int header4;
    int methodHeader1;
    int sz1;
    int fmt1;
    int header11;
    int header21;
    int type2;

	header = longAt(oop);
	header = (header & 4294967292U) | 2;
	if (oop >= youngStart) {
		header = header | 2147483648U;
	}
	longAtput(oop, header);
	parentField = 3;
	child = oop;
	/* begin lastPointerOf: */
	header2 = longAt(oop);
	fmt = (((unsigned) header2) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header2))) {
			lastFieldOffset = (6 + (fetchStackPointerOf(oop))) * 4;
			goto l6;
		}
		/* begin sizeBitsOfSafe: */
		header3 = longAt(oop);
		/* begin rightType: */
		if ((header3 & 252) == 0) {
			type1 = 0;
			goto l7;
		} else {
			if ((header3 & 126976) == 0) {
				type1 = 1;
				goto l7;
			} else {
				type1 = 3;
				goto l7;
			}
		}
	l7:	/* end rightType: */;
		if (type1 == 0) {
			sz = (longAt(oop - 8)) & 4294967292U;
			goto l8;
		} else {
			sz = header3 & 252;
			goto l8;
		}
	l8:	/* end sizeBitsOfSafe: */;
		lastFieldOffset = sz - 4;
		goto l6;
	}
	if (fmt < 12) {
		lastFieldOffset = 0;
		goto l6;
	}
	methodHeader = longAt(oop + 4);
	lastFieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
l6:	/* end lastPointerOf: */;
	field = oop + lastFieldOffset;
	action = 1;
	while (!(action == 4)) {
		if (action == 1) {
			/* begin startField */
			child = longAt(field);
			typeBits = child & 3;
			if ((typeBits & 1) == 1) {
				field -= 4;
				action = 1;
				goto l1;
			}
			if (typeBits == 0) {
				longAtput(field, parentField);
				parentField = field;
				action = 2;
				goto l1;
			}
			if (typeBits == 2) {
				if ((child & 126976) != 0) {
					child = child & 4294967292U;
					/* begin rightType: */
					if ((child & 252) == 0) {
						childType = 0;
						goto l2;
					} else {
						if ((child & 126976) == 0) {
							childType = 1;
							goto l2;
						} else {
							childType = 3;
							goto l2;
						}
					}
				l2:	/* end rightType: */;
					longAtput(field, child | childType);
					action = 3;
					goto l1;
				} else {
					child = longAt(field - 4);
					child = child & 4294967292U;
					longAtput(field - 4, parentField);
					parentField = (field - 4) | 1;
					action = 2;
					goto l1;
				}
			}
		l1:	/* end startField */;
		}
		if (action == 2) {
			/* begin startObj */
			oop1 = child;
			if (oop1 < youngStart) {
				field = oop1;
				action = 3;
				goto l12;
			}
			header4 = longAt(oop1);
			if ((header4 & 2147483648U) == 0) {
				if (((((unsigned) (longAt(oop1))) >> 8) & 15) == 4) {
					lastFieldOffset1 = (nonWeakFieldsOf(oop1)) << 2;
				} else {
					/* begin lastPointerOf: */
					header11 = longAt(oop1);
					fmt1 = (((unsigned) header11) >> 8) & 15;
					if (fmt1 <= 4) {
						if ((fmt1 == 3) && (isContextHeader(header11))) {
							lastFieldOffset1 = (6 + (fetchStackPointerOf(oop1))) * 4;
							goto l9;
						}
						/* begin sizeBitsOfSafe: */
						header21 = longAt(oop1);
						/* begin rightType: */
						if ((header21 & 252) == 0) {
							type2 = 0;
							goto l10;
						} else {
							if ((header21 & 126976) == 0) {
								type2 = 1;
								goto l10;
							} else {
								type2 = 3;
								goto l10;
							}
						}
					l10:	/* end rightType: */;
						if (type2 == 0) {
							sz1 = (longAt(oop1 - 8)) & 4294967292U;
							goto l11;
						} else {
							sz1 = header21 & 252;
							goto l11;
						}
					l11:	/* end sizeBitsOfSafe: */;
						lastFieldOffset1 = sz1 - 4;
						goto l9;
					}
					if (fmt1 < 12) {
						lastFieldOffset1 = 0;
						goto l9;
					}
					methodHeader1 = longAt(oop1 + 4);
					lastFieldOffset1 = (((((unsigned) methodHeader1) >> 10) & 255) * 4) + 4;
				l9:	/* end lastPointerOf: */;
				}
				header4 = header4 & 4294967292U;
				header4 = (header4 | 2147483648U) | 2;
				longAtput(oop1, header4);
				field = oop1 + lastFieldOffset1;
				action = 1;
				goto l12;
			} else {
				field = oop1;
				action = 3;
				goto l12;
			}
		l12:	/* end startObj */;
		}
		if (action == 3) {
			/* begin upward */
			if ((parentField & 1) == 1) {
				if (parentField == 3) {
					header1 = (longAt(field)) & 4294967292U;
					/* begin rightType: */
					if ((header1 & 252) == 0) {
						type = 0;
						goto l3;
					} else {
						if ((header1 & 126976) == 0) {
							type = 1;
							goto l3;
						} else {
							type = 3;
							goto l3;
						}
					}
				l3:	/* end rightType: */;
					longAtput(field, header1 + type);
					action = 4;
					goto l5;
				} else {
					child = field;
					field = parentField - 1;
					parentField = longAt(field);
					header1 = longAt(field + 4);
					/* begin rightType: */
					if ((header1 & 252) == 0) {
						type = 0;
						goto l4;
					} else {
						if ((header1 & 126976) == 0) {
							type = 1;
							goto l4;
						} else {
							type = 3;
							goto l4;
						}
					}
				l4:	/* end rightType: */;
					longAtput(field, child + type);
					field += 4;
					header1 = header1 & 4294967292U;
					longAtput(field, header1 + type);
					action = 3;
					goto l5;
				}
			} else {
				child = field;
				field = parentField;
				parentField = longAt(field);
				longAtput(field, child);
				field -= 4;
				action = 1;
				goto l5;
			}
		l5:	/* end upward */;
		}
	}
}

void markAndTraceInterpreterOops(void) {
    int i;
    int oop;

	markAndTrace(specialObjectsOop);
	markAndTrace(activeContext);
	markAndTrace(messageSelector);
	markAndTrace(newMethod);
	for (i = 1; i <= remapBufferCount; i += 1) {
		oop = remapBuffer[i];
		if (!((oop & 1))) {
			markAndTrace(oop);
		}
	}
}

void markPhase(void) {
    int oop;
    int i;
    int i1;
    int oop1;

	freeContexts = 1;
	/* begin markAndTraceInterpreterOops */
	markAndTrace(specialObjectsOop);
	markAndTrace(activeContext);
	markAndTrace(messageSelector);
	markAndTrace(newMethod);
	for (i1 = 1; i1 <= remapBufferCount; i1 += 1) {
		oop1 = remapBuffer[i1];
		if (!((oop1 & 1))) {
			markAndTrace(oop1);
		}
	}
	for (i = 1; i <= rootTableCount; i += 1) {
		oop = rootTable[i];
		if (!((oop & 1))) {
			markAndTrace(oop);
		}
	}
}

int mergewith(int sourceWord, int destinationWord) {
    int (*mergeFnwith)(int, int);

	mergeFnwith = ((int (*)(int, int)) (opTable[combinationRule + 1]));
	mergeFnwith;
	return mergeFnwith(sourceWord, destinationWord);
}

int methodClassOf(int methodPointer) {
	return longAt(((((char *) (longAt(((((char *) methodPointer)) + 4) + (((((((unsigned) (longAt(((((char *) methodPointer)) + 4) + (0 << 2)))) >> 10) & 255) - 1) + 1) << 2))))) + 4) + (1 << 2));
}

void newActiveContext(int aContext) {
    int tmp;

	/* begin storeContextRegisters: */
	longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
	longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
	if (aContext < youngStart) {
		beRootIfOld(aContext);
	}
	activeContext = aContext;
	/* begin fetchContextRegisters: */
	tmp = longAt(((((char *) aContext)) + 4) + (3 << 2));
	if ((tmp & 1)) {
		tmp = longAt(((((char *) aContext)) + 4) + (5 << 2));
		if (tmp < youngStart) {
			beRootIfOld(tmp);
		}
	} else {
		tmp = aContext;
	}
	theHomeContext = tmp;
	receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
	method = longAt(((((char *) tmp)) + 4) + (3 << 2));
	tmp = ((longAt(((((char *) aContext)) + 4) + (1 << 2))) >> 1);
	instructionPointer = ((method + tmp) + 4) - 2;
	tmp = ((longAt(((((char *) aContext)) + 4) + (2 << 2))) >> 1);
	stackPointer = (aContext + 4) + (((6 + tmp) - 1) * 4);
}

int newObjectHash(void) {
	lastHash = (13849 + (27181 * lastHash)) & 65535;
	return lastHash;
}

int nilObject(void) {
	return nilObj;
}

int nonWeakFieldsOf(int oop) {
    int classFormat;
    int cls;
    int ccIndex;

	if (!(((((unsigned) (longAt(oop))) >> 8) & 15) == 4)) {
		error("Called fixedFieldsOfWeak: with a non-weak oop");
	}
	/* begin fetchClassOf: */
	if ((oop & 1)) {
		cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l1;
	}
	ccIndex = (((unsigned) (longAt(oop))) >> 12) & 31;
	if (ccIndex == 0) {
		cls = (longAt(oop - 4)) & 4294967292U;
		goto l1;
	} else {
		cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l1;
	}
l1:	/* end fetchClassOf: */;
	classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
	return (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
}

int objectAfter(int oop) {
    int sz;
    int header;
    int extra;
    int type;
    int extra1;

	;
	if (((longAt(oop)) & 3) == 2) {
		sz = (longAt(oop)) & 4294967292U;
	} else {
		/* begin sizeBitsOf: */
		header = longAt(oop);
		if ((header & 3) == 0) {
			sz = (longAt(oop - 8)) & 4294967292U;
			goto l1;
		} else {
			sz = header & 252;
			goto l1;
		}
	l1:	/* end sizeBitsOf: */;
	}
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type = (longAt(oop + sz)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	return (oop + sz) + extra;
}

int objectAfterWhileForwarding(int oop) {
    int sz;
    int fwdBlock;
    int realHeader;
    int header;
    int extra;
    int type;
    int extra1;
    int sz1;
    int header1;
    int extra2;
    int type1;
    int extra11;

	header = longAt(oop);
	if ((header & 2147483648U) == 0) {
		/* begin objectAfter: */
		;
		if (((longAt(oop)) & 3) == 2) {
			sz1 = (longAt(oop)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header1 = longAt(oop);
			if ((header1 & 3) == 0) {
				sz1 = (longAt(oop - 8)) & 4294967292U;
				goto l1;
			} else {
				sz1 = header1 & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(oop + sz1)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		return (oop + sz1) + extra2;
	}
	fwdBlock = (header & 2147483644) << 1;
	;
	realHeader = longAt(fwdBlock + 4);
	if ((realHeader & 3) == 0) {
		sz = (longAt(oop - 8)) & 4294967292U;
	} else {
		sz = realHeader & 252;
	}
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type = (longAt(oop + sz)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	return (oop + sz) + extra;
}

void okayActiveProcessStack(void) {
    int cntxt;

	cntxt = activeContext;
	while (!(cntxt == nilObj)) {
		okayFields(cntxt);
		cntxt = longAt(((((char *) cntxt)) + 4) + (0 << 2));
	}
}

int okayFields(int oop) {
    int i;
    int fieldOop;

	if ((oop == null) || (oop == 0)) {
		return 1;
	}
	if ((oop & 1)) {
		return 1;
	}
	okayOop(oop);
	oopHasOkayClass(oop);
	if (!(((((unsigned) (longAt(oop))) >> 8) & 15) <= 4)) {
		return 1;
	}
	i = (lengthOf(oop)) - 1;
	while (i >= 0) {
		fieldOop = longAt(((((char *) oop)) + 4) + (i << 2));
		if (!((fieldOop & 1))) {
			okayOop(fieldOop);
			oopHasOkayClass(fieldOop);
		}
		i -= 1;
	}
}

void okayInterpreterObjects(void) {
    int i;
    int oop;
    int oopOrZero;
    int cntxt;

	okayFields(nilObj);
	okayFields(falseObj);
	okayFields(trueObj);
	okayFields(specialObjectsOop);
	okayFields(activeContext);
	okayFields(method);
	okayFields(receiver);
	okayFields(theHomeContext);
	okayFields(messageSelector);
	okayFields(newMethod);
	for (i = 0; i <= (256 - 1); i += 4) {
		oopOrZero = methodCache[i + 1];
		if (!(oopOrZero == 0)) {
			okayFields(methodCache[i + 1]);
			okayFields(methodCache[i + 2]);
			okayFields(methodCache[i + 3]);
		}
	}
	for (i = 1; i <= remapBufferCount; i += 1) {
		oop = remapBuffer[i];
		if (!((oop & 1))) {
			okayFields(oop);
		}
	}
	/* begin okayActiveProcessStack */
	cntxt = activeContext;
	while (!(cntxt == nilObj)) {
		okayFields(cntxt);
		cntxt = longAt(((((char *) cntxt)) + 4) + (0 << 2));
	}
}

int okayOop(int oop) {
    int sz;
    int type;
    int fmt;
    int header;

	if ((oop & 1)) {
		return 1;
	}
	if (!((0 < oop) && (oop < endOfMemory))) {
		error("oop is not a valid address");
	}
	if (!((oop % 4) == 0)) {
		error("oop is not a word-aligned address");
	}
	/* begin sizeBitsOf: */
	header = longAt(oop);
	if ((header & 3) == 0) {
		sz = (longAt(oop - 8)) & 4294967292U;
		goto l1;
	} else {
		sz = header & 252;
		goto l1;
	}
l1:	/* end sizeBitsOf: */;
	if (!((oop + sz) < endOfMemory)) {
		error("oop size would make it extend beyond the end of memory");
	}
	type = (longAt(oop)) & 3;
	if (type == 2) {
		error("oop is a free chunk, not an object");
	}
	if (type == 3) {
		if (((((unsigned) (longAt(oop))) >> 12) & 31) == 0) {
			error("cannot have zero compact class field in a short header");
		}
	}
	if (type == 1) {
		if (!((oop >= 4) && (((longAt(oop - 4)) & 3) == type))) {
			error("class header word has wrong type");
		}
	}
	if (type == 0) {
		if (!((oop >= 8) && ((((longAt(oop - 8)) & 3) == type) && (((longAt(oop - 4)) & 3) == type)))) {
			error("class header word has wrong type");
		}
	}
	fmt = (((unsigned) (longAt(oop))) >> 8) & 15;
	if ((fmt == 5) || (fmt == 7)) {
		error("oop has an unknown format type");
	}
	if (!(((longAt(oop)) & 536870912) == 0)) {
		error("unused header bit 30 is set; should be zero");
	}
	if ((((longAt(oop)) & 1073741824) == 1) && (oop >= youngStart)) {
		error("root bit is set in a young object");
	}
	return 1;
}

int oopFromChunk(int chunk) {
    int extra;
    int type;
    int extra1;

	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	return chunk + extra;
}

int oopHasOkayClass(int oop) {
    int behaviorFormatBits;
    int oopClass;
    int formatMask;
    int oopFormatBits;
    int ccIndex;

	okayOop(oop);
	/* begin fetchClassOf: */
	if ((oop & 1)) {
		oopClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l1;
	}
	ccIndex = (((unsigned) (longAt(oop))) >> 12) & 31;
	if (ccIndex == 0) {
		oopClass = (longAt(oop - 4)) & 4294967292U;
		goto l1;
	} else {
		oopClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l1;
	}
l1:	/* end fetchClassOf: */;
	if ((oopClass & 1)) {
		error("a SmallInteger is not a valid class or behavior");
	}
	okayOop(oopClass);
	if (!((((((unsigned) (longAt(oopClass))) >> 8) & 15) <= 4) && ((lengthOf(oopClass)) >= 3))) {
		error("a class (behavior) must be a pointers object of size >= 3");
	}
	if (((((unsigned) (longAt(oop))) >> 8) & 15) >= 8) {
		formatMask = 3072;
	} else {
		formatMask = 3840;
	}
	behaviorFormatBits = ((longAt(((((char *) oopClass)) + 4) + (2 << 2))) - 1) & formatMask;
	oopFormatBits = (longAt(oop)) & formatMask;
	if (!(behaviorFormatBits == oopFormatBits)) {
		error("object and its class (behavior) formats differ");
	}
	return 1;
}

int partitionedANDtonBitsnPartitions(int word1, int word2, int nBits, int nParts) {
    int mask;
    int i;
    int result;

	mask = maskTable[nBits];
	result = 0;
	for (i = 1; i <= nParts; i += 1) {
		if ((word1 & mask) == mask) {
			result = result | (word2 & mask);
		}
		mask = mask << nBits;
	}
	return result;
}

int partitionedAddtonBitsnPartitions(int word1, int word2, int nBits, int nParts) {
    int mask;
    int sum;
    int i;
    int result;

	mask = maskTable[nBits];
	result = 0;
	for (i = 1; i <= nParts; i += 1) {
		sum = (word1 & mask) + (word2 & mask);
		if (sum <= mask) {
			result = result | sum;
		} else {
			result = result | mask;
		}
		mask = mask << nBits;
	}
	return result;
}

int partitionedMaxwithnBitsnPartitions(int word1, int word2, int nBits, int nParts) {
    int mask;
    int i;
    int result;

	mask = maskTable[nBits];
	result = 0;
	for (i = 1; i <= nParts; i += 1) {
		result = result | ((((word2 & mask) < (word1 & mask)) ? (word1 & mask) : (word2 & mask)));
		mask = mask << nBits;
	}
	return result;
}

int partitionedMinwithnBitsnPartitions(int word1, int word2, int nBits, int nParts) {
    int mask;
    int i;
    int result;

	mask = maskTable[nBits];
	result = 0;
	for (i = 1; i <= nParts; i += 1) {
		result = result | ((((word2 & mask) < (word1 & mask)) ? (word2 & mask) : (word1 & mask)));
		mask = mask << nBits;
	}
	return result;
}

int partitionedSubfromnBitsnPartitions(int word1, int word2, int nBits, int nParts) {
    int mask;
    int i;
    int p1;
    int p2;
    int result;

	mask = maskTable[nBits];
	result = 0;
	for (i = 1; i <= nParts; i += 1) {
		p1 = word1 & mask;
		p2 = word2 & mask;
		if (p1 < p2) {
			result = result | (p2 - p1);
		} else {
			result = result | (p1 - p2);
		}
		mask = mask << nBits;
	}
	return result;
}

int pickSourcePixelsnullMapsrcMaskdestMask(int nPix, int nullMap, int sourcePixMask, int destPixMask) {
	if (sourcePixSize >= 16) {
		return pickSourcePixelsRGBnullMapsrcMaskdestMask(nPix, nullMap, sourcePixMask, destPixMask);
	}
	if (nullMap) {
		return pickSourcePixelsNullMapsrcMaskdestMask(nPix, sourcePixMask, destPixMask);
	}
	return pickSourcePixelssrcMaskdestMask(nPix, sourcePixMask, destPixMask);
}

int pickSourcePixelssrcMaskdestMask(int nPix, int sourcePixMask, int destPixMask) {
    int sourceWord;
    int destWord;
    int sourcePix;
    int destPix;
    int i;

	sourceWord = longAt(sourceIndex);
	destWord = 0;
	for (i = 1; i <= nPix; i += 1) {
		sourcePix = (((unsigned) sourceWord) >> ((32 - sourcePixSize) - srcBitIndex)) & sourcePixMask;
		destPix = (longAt(((((char *) colorMap)) + 4) + (sourcePix << 2))) & destPixMask;
		if (destPixSize == 32) {
			destWord = destPix;
		} else {
			destWord = (destWord << destPixSize) | destPix;
		}
		if ((srcBitIndex += sourcePixSize) > 31) {
			srcBitIndex -= 32;
			sourceIndex += 4;
			sourceWord = longAt(sourceIndex);
		}
	}
	return destWord;
}

int pickSourcePixelsNullMapsrcMaskdestMask(int nPix, int sourcePixMask, int destPixMask) {
    int sourceWord;
    int destWord;
    int sourcePix;
    int i;

	sourceWord = longAt(sourceIndex);
	destWord = 0;
	for (i = 1; i <= nPix; i += 1) {
		sourcePix = (((unsigned) sourceWord) >> ((32 - sourcePixSize) - srcBitIndex)) & sourcePixMask;
		if (destPixSize == 32) {
			destWord = sourcePix;
		} else {
			destWord = (destWord << destPixSize) | (sourcePix & destPixMask);
		}
		if ((srcBitIndex += sourcePixSize) > 31) {
			srcBitIndex -= 32;
			sourceIndex += 4;
			sourceWord = longAt(sourceIndex);
		}
	}
	return destWord;
}

int pickSourcePixelsRGBnullMapsrcMaskdestMask(int nPix, int nullMap, int sourcePixMask, int destPixMask) {
    int sourceWord;
    int destWord;
    int sourcePix;
    int destPix;
    int i;
    int mask;
    int srcPix;
    int destPix1;
    int d;
    int mask3;
    int srcPix1;
    int destPix2;
    int d1;
    int mask4;
    int srcPix2;
    int destPix3;
    int d2;
    int mask5;
    int srcPix3;
    int destPix4;
    int d3;

	sourceWord = longAt(sourceIndex);
	destWord = 0;
	for (i = 1; i <= nPix; i += 1) {
		sourcePix = (((unsigned) sourceWord) >> ((32 - sourcePixSize) - srcBitIndex)) & sourcePixMask;
		if (nullMap) {
			if (sourcePixSize == 16) {
				/* begin rgbMap:from:to: */
				if ((d = 8 - 5) > 0) {
					mask = (1 << 5) - 1;
					srcPix = sourcePix << d;
					mask = mask << d;
					destPix1 = srcPix & mask;
					mask = mask << 8;
					srcPix = srcPix << d;
					destPix = (destPix1 + (srcPix & mask)) + ((srcPix << d) & (mask << 8));
					goto l1;
				} else {
					if (d == 0) {
						destPix = sourcePix;
						goto l1;
					}
					if (sourcePix == 0) {
						destPix = sourcePix;
						goto l1;
					}
					d = 5 - 8;
					mask = (1 << 8) - 1;
					srcPix = ((unsigned) sourcePix) >> d;
					destPix1 = srcPix & mask;
					mask = mask << 8;
					srcPix = ((unsigned) srcPix) >> d;
					destPix1 = (destPix1 + (srcPix & mask)) + ((((unsigned) srcPix) >> d) & (mask << 8));
					if (destPix1 == 0) {
						destPix = 1;
						goto l1;
					}
					destPix = destPix1;
					goto l1;
				}
			l1:	/* end rgbMap:from:to: */;
			} else {
				/* begin rgbMap:from:to: */
				if ((d1 = 5 - 8) > 0) {
					mask3 = (1 << 8) - 1;
					srcPix1 = sourcePix << d1;
					mask3 = mask3 << d1;
					destPix2 = srcPix1 & mask3;
					mask3 = mask3 << 5;
					srcPix1 = srcPix1 << d1;
					destPix = (destPix2 + (srcPix1 & mask3)) + ((srcPix1 << d1) & (mask3 << 5));
					goto l2;
				} else {
					if (d1 == 0) {
						destPix = sourcePix;
						goto l2;
					}
					if (sourcePix == 0) {
						destPix = sourcePix;
						goto l2;
					}
					d1 = 8 - 5;
					mask3 = (1 << 5) - 1;
					srcPix1 = ((unsigned) sourcePix) >> d1;
					destPix2 = srcPix1 & mask3;
					mask3 = mask3 << 5;
					srcPix1 = ((unsigned) srcPix1) >> d1;
					destPix2 = (destPix2 + (srcPix1 & mask3)) + ((((unsigned) srcPix1) >> d1) & (mask3 << 5));
					if (destPix2 == 0) {
						destPix = 1;
						goto l2;
					}
					destPix = destPix2;
					goto l2;
				}
			l2:	/* end rgbMap:from:to: */;
			}
		} else {
			if (sourcePixSize == 16) {
				/* begin rgbMap:from:to: */
				if ((d2 = cmBitsPerColor - 5) > 0) {
					mask4 = (1 << 5) - 1;
					srcPix2 = sourcePix << d2;
					mask4 = mask4 << d2;
					destPix3 = srcPix2 & mask4;
					mask4 = mask4 << cmBitsPerColor;
					srcPix2 = srcPix2 << d2;
					sourcePix = (destPix3 + (srcPix2 & mask4)) + ((srcPix2 << d2) & (mask4 << cmBitsPerColor));
					goto l3;
				} else {
					if (d2 == 0) {
						sourcePix = sourcePix;
						goto l3;
					}
					if (sourcePix == 0) {
						sourcePix = sourcePix;
						goto l3;
					}
					d2 = 5 - cmBitsPerColor;
					mask4 = (1 << cmBitsPerColor) - 1;
					srcPix2 = ((unsigned) sourcePix) >> d2;
					destPix3 = srcPix2 & mask4;
					mask4 = mask4 << cmBitsPerColor;
					srcPix2 = ((unsigned) srcPix2) >> d2;
					destPix3 = (destPix3 + (srcPix2 & mask4)) + ((((unsigned) srcPix2) >> d2) & (mask4 << cmBitsPerColor));
					if (destPix3 == 0) {
						sourcePix = 1;
						goto l3;
					}
					sourcePix = destPix3;
					goto l3;
				}
			l3:	/* end rgbMap:from:to: */;
			} else {
				/* begin rgbMap:from:to: */
				if ((d3 = cmBitsPerColor - 8) > 0) {
					mask5 = (1 << 8) - 1;
					srcPix3 = sourcePix << d3;
					mask5 = mask5 << d3;
					destPix4 = srcPix3 & mask5;
					mask5 = mask5 << cmBitsPerColor;
					srcPix3 = srcPix3 << d3;
					sourcePix = (destPix4 + (srcPix3 & mask5)) + ((srcPix3 << d3) & (mask5 << cmBitsPerColor));
					goto l4;
				} else {
					if (d3 == 0) {
						sourcePix = sourcePix;
						goto l4;
					}
					if (sourcePix == 0) {
						sourcePix = sourcePix;
						goto l4;
					}
					d3 = 8 - cmBitsPerColor;
					mask5 = (1 << cmBitsPerColor) - 1;
					srcPix3 = ((unsigned) sourcePix) >> d3;
					destPix4 = srcPix3 & mask5;
					mask5 = mask5 << cmBitsPerColor;
					srcPix3 = ((unsigned) srcPix3) >> d3;
					destPix4 = (destPix4 + (srcPix3 & mask5)) + ((((unsigned) srcPix3) >> d3) & (mask5 << cmBitsPerColor));
					if (destPix4 == 0) {
						sourcePix = 1;
						goto l4;
					}
					sourcePix = destPix4;
					goto l4;
				}
			l4:	/* end rgbMap:from:to: */;
			}
			destPix = (longAt(((((char *) colorMap)) + 4) + (sourcePix << 2))) & destPixMask;
		}
		if (destPixSize == 32) {
			destWord = destPix;
		} else {
			destWord = (destWord << destPixSize) | destPix;
		}
		if ((srcBitIndex += sourcePixSize) > 31) {
			srcBitIndex -= 32;
			sourceIndex += 4;
			sourceWord = longAt(sourceIndex);
		}
	}
	return destWord;
}

int pixMaskwith(int sourceWord, int destinationWord) {
    int mask;
    int i;
    int result;

	/* begin partitionedAND:to:nBits:nPartitions: */
	mask = maskTable[destPixSize];
	result = 0;
	for (i = 1; i <= pixPerWord; i += 1) {
		if (((~sourceWord) & mask) == mask) {
			result = result | (destinationWord & mask);
		}
		mask = mask << destPixSize;
	}
	return result;
}

int pixPaintwith(int sourceWord, int destinationWord) {
	if (sourceWord == 0) {
		return destinationWord;
	}
	return sourceWord | (partitionedANDtonBitsnPartitions(~sourceWord, destinationWord, destPixSize, pixPerWord));
}

void pop2AndPushIntegerIfOK(int integerResult) {
    int sp;

	if (successFlag) {
		if ((integerResult ^ (integerResult << 1)) >= 0) {
			/* begin pop:thenPush: */
			longAtput(sp = stackPointer - ((2 - 1) * 4), ((integerResult << 1) | 1));
			stackPointer = sp;
		} else {
			successFlag = 0;
		}
	}
}

void pop(int nItems) {
	stackPointer -= nItems * 4;
}

int popthenPush(int nItems, int oop) {
    int sp;

	longAtput(sp = stackPointer - ((nItems - 1) * 4), oop);
	return stackPointer = sp;
}

double popFloat(void) {
    int top;
    double result;
    int top1;

	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	top = top1;
	if (successFlag) {
		fetchFloatAtinto(top + 4, result);
	}
	return result;
}

int popInteger(void) {
    int integerPointer;
    int top;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		return (integerPointer >> 1);
	} else {
		successFlag = 0;
		return 1;
	}
}

int popPos32BitInteger(void) {
    int top;
    int top1;

	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	top = top1;
	return positive32BitValueOf(top);
}

int popRemappableOop(void) {
    int oop;

	oop = remapBuffer[remapBufferCount];
	remapBufferCount -= 1;
	return oop;
}

int popStack(void) {
    int top;

	top = longAt(stackPointer);
	stackPointer -= 4;
	return top;
}

int positive32BitIntegerFor(int integerValue) {
    int newLargeInteger;

	if ((integerValue >= 0) && ((integerValue ^ (integerValue << 1)) >= 0)) {
		return ((integerValue << 1) | 1);
	}
	newLargeInteger = instantiateSmallClasssizeInBytesfill(longAt(((((char *) specialObjectsOop)) + 4) + (13 << 2)), 8, 0);
	byteAtput(((((char *) newLargeInteger)) + 4) + 3, (((unsigned) integerValue) >> 24) & 255);
	byteAtput(((((char *) newLargeInteger)) + 4) + 2, (((unsigned) integerValue) >> 16) & 255);
	byteAtput(((((char *) newLargeInteger)) + 4) + 1, (((unsigned) integerValue) >> 8) & 255);
	byteAtput(((((char *) newLargeInteger)) + 4) + 0, integerValue & 255);
	return newLargeInteger;
}

int positive32BitValueOf(int oop) {
    int sz;
    int value;
    int header;
    int sz1;

	if ((oop & 1)) {
		value = (oop >> 1);
		if (value < 0) {
			return successFlag = 0;
		}
		return value;
	}
	if (successFlag) {
		/* begin lengthOf: */
		header = longAt(oop);
		/* begin lengthOf:baseHeader:format: */
		if ((header & 3) == 0) {
			sz1 = (longAt(oop - 8)) & 4294967292U;
		} else {
			sz1 = header & 252;
		}
		if (((((unsigned) header) >> 8) & 15) < 8) {
			sz = ((unsigned) (sz1 - 4)) >> 2;
			goto l1;
		} else {
			sz = (sz1 - 4) - (((((unsigned) header) >> 8) & 15) & 3);
			goto l1;
		}
	l1:	/* end lengthOf:baseHeader:format: */;
		if (!(sz == 4)) {
			return successFlag = 0;
		}
	}
	if (successFlag) {
		return (((byteAt(((((char *) oop)) + 4) + 0)) + ((byteAt(((((char *) oop)) + 4) + 1)) << 8)) + ((byteAt(((((char *) oop)) + 4) + 2)) << 16)) + ((byteAt(((((char *) oop)) + 4) + 3)) << 24);
	}
}

void possibleRootStoreIntovalue(int oop, int valueObj) {
    int header;

	if ((valueObj >= youngStart) && (!((valueObj & 1)))) {
		header = longAt(oop);
		if ((header & 1073741824) == 0) {
			if (rootTableCount < 500) {
				rootTableCount += 1;
				rootTable[rootTableCount] = oop;
				longAtput(oop, header | 1073741824);
			}
		}
	}
}

void postGCAction(void) {
	if (activeContext < youngStart) {
		beRootIfOld(activeContext);
	}
	if (theHomeContext < youngStart) {
		beRootIfOld(theHomeContext);
	}
}

void preGCAction(void) {
	if (!(activeContext == nilObj)) {
		/* begin storeContextRegisters: */
		longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
		longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
	}
}

int prepareForwardingTableForBecomingwithtwoWay(int array1, int array2, int twoWayFlag) {
    int entriesAvailable;
    int fwdBlock;
    int fieldOffset;
    int oop1;
    int oop2;
    int entriesNeeded;
    int fwdBlkSize;
    int originalHeader;
    int originalHeaderType;
    int originalHeader1;
    int originalHeaderType1;
    int methodHeader;
    int sz;
    int fmt;
    int header;
    int header1;
    int type;

	entriesNeeded = ((int) (lastPointerOf(array1)) >> 2);
	if (twoWayFlag) {
		entriesNeeded = entriesNeeded * 2;
		fwdBlkSize = 8;
	} else {
		fwdBlkSize = 16;
	}
	entriesAvailable = fwdTableInit(fwdBlkSize);
	if (entriesAvailable < entriesNeeded) {
		initializeMemoryFirstFree(freeBlock);
		return 0;
	}
	/* begin lastPointerOf: */
	header = longAt(array1);
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			fieldOffset = (6 + (fetchStackPointerOf(array1))) * 4;
			goto l3;
		}
		/* begin sizeBitsOfSafe: */
		header1 = longAt(array1);
		/* begin rightType: */
		if ((header1 & 252) == 0) {
			type = 0;
			goto l4;
		} else {
			if ((header1 & 126976) == 0) {
				type = 1;
				goto l4;
			} else {
				type = 3;
				goto l4;
			}
		}
	l4:	/* end rightType: */;
		if (type == 0) {
			sz = (longAt(array1 - 8)) & 4294967292U;
			goto l5;
		} else {
			sz = header1 & 252;
			goto l5;
		}
	l5:	/* end sizeBitsOfSafe: */;
		fieldOffset = sz - 4;
		goto l3;
	}
	if (fmt < 12) {
		fieldOffset = 0;
		goto l3;
	}
	methodHeader = longAt(array1 + 4);
	fieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
l3:	/* end lastPointerOf: */;
	while (fieldOffset >= 4) {
		oop1 = longAt(array1 + fieldOffset);
		oop2 = longAt(array2 + fieldOffset);
		/* begin fwdBlockGet: */
		fwdTableNext += fwdBlkSize;
		if (fwdTableNext <= fwdTableLast) {
			fwdBlock = fwdTableNext;
			goto l2;
		} else {
			fwdBlock = null;
			goto l2;
		}
	l2:	/* end fwdBlockGet: */;
		/* begin initForwardBlock:mapping:to:withBackPtr: */
		originalHeader1 = longAt(oop1);
		;
		originalHeaderType1 = originalHeader1 & 3;
		longAtput(fwdBlock, oop2);
		longAtput(fwdBlock + 4, originalHeader1);
		if (!twoWayFlag) {
			longAtput(fwdBlock + 8, oop1);
		}
		longAtput(oop1, (((unsigned) fwdBlock) >> 1) | (2147483648U | originalHeaderType1));
		if (twoWayFlag) {
			/* begin fwdBlockGet: */
			fwdTableNext += fwdBlkSize;
			if (fwdTableNext <= fwdTableLast) {
				fwdBlock = fwdTableNext;
				goto l1;
			} else {
				fwdBlock = null;
				goto l1;
			}
		l1:	/* end fwdBlockGet: */;
			/* begin initForwardBlock:mapping:to:withBackPtr: */
			originalHeader = longAt(oop2);
			;
			originalHeaderType = originalHeader & 3;
			longAtput(fwdBlock, oop1);
			longAtput(fwdBlock + 4, originalHeader);
			if (!twoWayFlag) {
				longAtput(fwdBlock + 8, oop2);
			}
			longAtput(oop2, (((unsigned) fwdBlock) >> 1) | (2147483648U | originalHeaderType));
		}
		fieldOffset -= 4;
	}
	return 1;
}

int primIndex(void) {
	return primitiveIndex;
}

void primitiveAdd(void) {
    int integerResult;
    int sp;

	/* begin pop2AndPushIntegerIfOK: */
	integerResult = (stackIntegerValue(1)) + (stackIntegerValue(0));
	if (successFlag) {
		if ((integerResult ^ (integerResult << 1)) >= 0) {
			/* begin pop:thenPush: */
			longAtput(sp = stackPointer - ((2 - 1) * 4), ((integerResult << 1) | 1));
			stackPointer = sp;
		} else {
			successFlag = 0;
		}
	}
}

void primitiveArctan(void) {
    double rcvr;

	rcvr = popFloat();
	if (successFlag) {
		pushFloat(atan(rcvr));
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveArrayBecome(void) {
    int arg;
    int rcvr;
    int successValue;
    int i;

	arg = longAt(stackPointer);
	rcvr = longAt(stackPointer - (1 * 4));
	/* begin success: */
	successValue = becomewithtwoWay(rcvr, arg, 1);
	successFlag = successValue && successFlag;
	/* begin flushMethodCache */
	for (i = 1; i <= 1024; i += 1) {
		methodCache[i] = 0;
	}
	for (i = 1; i <= 64; i += 1) {
		atCache[i] = 0;
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 1 * 4;
	}
}

void primitiveArrayBecomeOneWay(void) {
    int arg;
    int rcvr;
    int successValue;
    int i;

	arg = longAt(stackPointer);
	rcvr = longAt(stackPointer - (1 * 4));
	/* begin success: */
	successValue = becomewithtwoWay(rcvr, arg, 0);
	successFlag = successValue && successFlag;
	/* begin flushMethodCache */
	for (i = 1; i <= 1024; i += 1) {
		methodCache[i] = 0;
	}
	for (i = 1; i <= 64; i += 1) {
		atCache[i] = 0;
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 1 * 4;
	}
}

void primitiveAsFloat(void) {
    int arg;
    int integerPointer;
    int top;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		arg = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		arg = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	if (successFlag) {
		pushFloat(((double) arg));
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveAsOop(void) {
    int thisReceiver;
    int sp;
    int top;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	thisReceiver = top;
	successFlag = (!((thisReceiver & 1))) && successFlag;
	if (successFlag) {
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp = stackPointer + 4, ((((((unsigned) (longAt(thisReceiver))) >> 17) & 4095) << 1) | 1));
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveAt(void) {
	commonAt(0);
}

void primitiveAtEnd(void) {
    int stream;
    int index;
    int limit;
    int sp;
    int sp1;
    int top;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	stream = top;
	successFlag = (((((unsigned) (longAt(stream))) >> 8) & 15) <= 4) && ((lengthOf(stream)) >= (2 + 1));
	if (successFlag) {
		index = fetchIntegerofObject(1, stream);
		limit = fetchIntegerofObject(2, stream);
	}
	if (successFlag) {
		/* begin pushBool: */
		if (index >= limit) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveAtPut(void) {
	commonAtPut(0);
}

void primitiveBeCursor(void) {
    int maskObj;
    int maskBitsIndex;
    int depth;
    int bitsObj;
    int extentX;
    int extentY;
    int cursorObj;
    int offsetObj;
    int offsetX;
    int offsetY;
    int cursorBitsIndex;
    int successValue;
    int successValue1;
    int successValue2;
    int successValue3;
    int successValue4;
    int successValue5;
    int successValue6;
    int successValue7;
    int successValue8;

	if (argumentCount == 0) {
		cursorObj = longAt(stackPointer);
		maskBitsIndex = null;
	}
	if (argumentCount == 1) {
		cursorObj = longAt(stackPointer - (1 * 4));
		maskObj = longAt(stackPointer);
	}
	successFlag = (argumentCount < 2) && successFlag;
	/* begin success: */
	successValue7 = (((((unsigned) (longAt(cursorObj))) >> 8) & 15) <= 4) && ((lengthOf(cursorObj)) >= 5);
	successFlag = successValue7 && successFlag;
	if (successFlag) {
		bitsObj = longAt(((((char *) cursorObj)) + 4) + (0 << 2));
		extentX = fetchIntegerofObject(1, cursorObj);
		extentY = fetchIntegerofObject(2, cursorObj);
		depth = fetchIntegerofObject(3, cursorObj);
		offsetObj = longAt(((((char *) cursorObj)) + 4) + (4 << 2));
	}
	/* begin success: */
	successValue8 = (((((unsigned) (longAt(offsetObj))) >> 8) & 15) <= 4) && ((lengthOf(offsetObj)) >= 2);
	successFlag = successValue8 && successFlag;
	if (successFlag) {
		offsetX = fetchIntegerofObject(0, offsetObj);
		offsetY = fetchIntegerofObject(1, offsetObj);
		/* begin success: */
		successValue = (extentX == 16) && ((extentY == 16) && (depth == 1));
		successFlag = successValue && successFlag;
		/* begin success: */
		successValue1 = (offsetX >= -16) && (offsetX <= 0);
		successFlag = successValue1 && successFlag;
		/* begin success: */
		successValue2 = (offsetY >= -16) && (offsetY <= 0);
		successFlag = successValue2 && successFlag;
		/* begin success: */
		successValue3 = (((((unsigned) (longAt(bitsObj))) >> 8) & 15) == 6) && ((lengthOf(bitsObj)) == 16);
		successFlag = successValue3 && successFlag;
		cursorBitsIndex = bitsObj + 4;
	}
	if (argumentCount == 1) {
		/* begin success: */
		successValue6 = (((((unsigned) (longAt(maskObj))) >> 8) & 15) <= 4) && ((lengthOf(maskObj)) >= 5);
		successFlag = successValue6 && successFlag;
		if (successFlag) {
			bitsObj = longAt(((((char *) maskObj)) + 4) + (0 << 2));
			extentX = fetchIntegerofObject(1, maskObj);
			extentY = fetchIntegerofObject(2, maskObj);
			depth = fetchIntegerofObject(3, maskObj);
		}
		if (successFlag) {
			/* begin success: */
			successValue4 = (extentX == 16) && ((extentY == 16) && (depth == 1));
			successFlag = successValue4 && successFlag;
			/* begin success: */
			successValue5 = (((((unsigned) (longAt(bitsObj))) >> 8) & 15) == 6) && ((lengthOf(bitsObj)) == 16);
			successFlag = successValue5 && successFlag;
			maskBitsIndex = bitsObj + 4;
		}
	}
	if (successFlag) {
		if (argumentCount == 0) {
			ioSetCursorWithMask(cursorBitsIndex, null, offsetX, offsetY);
		} else {
			ioSetCursorWithMask(cursorBitsIndex, maskBitsIndex, offsetX, offsetY);
		}
		/* begin pop: */
		stackPointer -= argumentCount * 4;
	}
}

void primitiveBeDisplay(void) {
    int rcvr;
    int oop;
    int successValue;

	rcvr = longAt(stackPointer);
	/* begin success: */
	successValue = (((((unsigned) (longAt(rcvr))) >> 8) & 15) <= 4) && ((lengthOf(rcvr)) >= 4);
	successFlag = successValue && successFlag;
	if (successFlag) {
		/* begin storePointer:ofObject:withValue: */
		oop = specialObjectsOop;
		if (oop < youngStart) {
			possibleRootStoreIntovalue(oop, rcvr);
		}
		longAtput(((((char *) oop)) + 4) + (14 << 2), rcvr);
	}
}

void primitiveBeep(void) {
	ioBeep();
}

void primitiveBitAnd(void) {
    int integerReceiver;
    int integerArgument;
    int object;
    int sp;
    int top;
    int top1;
    int top2;
    int top11;

	/* begin popPos32BitInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	top = top1;
	integerArgument = positive32BitValueOf(top);
	/* begin popPos32BitInteger */
	/* begin popStack */
	top11 = longAt(stackPointer);
	stackPointer -= 4;
	top2 = top11;
	integerReceiver = positive32BitValueOf(top2);
	if (successFlag) {
		/* begin push: */
		object = positive32BitIntegerFor(integerReceiver & integerArgument);
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveBitOr(void) {
    int integerReceiver;
    int integerArgument;
    int object;
    int sp;
    int top;
    int top1;
    int top2;
    int top11;

	/* begin popPos32BitInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	top = top1;
	integerArgument = positive32BitValueOf(top);
	/* begin popPos32BitInteger */
	/* begin popStack */
	top11 = longAt(stackPointer);
	stackPointer -= 4;
	top2 = top11;
	integerReceiver = positive32BitValueOf(top2);
	if (successFlag) {
		/* begin push: */
		object = positive32BitIntegerFor(integerReceiver | integerArgument);
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveBitShift(void) {
    int shifted;
    int integerReceiver;
    int integerArgument;
    int object;
    int sp;
    int integerPointer;
    int top;
    int top2;
    int top1;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		integerArgument = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerArgument = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popPos32BitInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	top2 = top1;
	integerReceiver = positive32BitValueOf(top2);
	if (successFlag) {
		if (integerArgument >= 0) {
			successFlag = (integerArgument <= 31) && successFlag;
			shifted = integerReceiver << integerArgument;
			successFlag = ((((unsigned) shifted) >> integerArgument) == integerReceiver) && successFlag;
		} else {
			successFlag = (integerArgument >= -31) && successFlag;
			shifted = ((integerArgument < 0) ? ((unsigned) integerReceiver >> -integerArgument) : ((unsigned) integerReceiver << integerArgument));
		}
	}
	if (successFlag) {
		/* begin push: */
		object = positive32BitIntegerFor(shifted);
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveBitXor(void) {
    int integerReceiver;
    int integerArgument;
    int object;
    int sp;
    int top;
    int top1;
    int top2;
    int top11;

	/* begin popPos32BitInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	top = top1;
	integerArgument = positive32BitValueOf(top);
	/* begin popPos32BitInteger */
	/* begin popStack */
	top11 = longAt(stackPointer);
	stackPointer -= 4;
	top2 = top11;
	integerReceiver = positive32BitValueOf(top2);
	if (successFlag) {
		/* begin push: */
		object = positive32BitIntegerFor(integerReceiver ^ integerArgument);
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveBlockCopy(void) {
    int methodContext;
    int newContext;
    int initialIP;
    int context;
    int contextSize;
    int header;
    int oop;
    int sp;

	context = longAt(stackPointer - (1 * 4));
	if (((longAt(((((char *) context)) + 4) + (3 << 2))) & 1)) {
		methodContext = longAt(((((char *) context)) + 4) + (5 << 2));
	} else {
		methodContext = context;
	}
	/* begin sizeBitsOf: */
	header = longAt(methodContext);
	if ((header & 3) == 0) {
		contextSize = (longAt(methodContext - 8)) & 4294967292U;
		goto l1;
	} else {
		contextSize = header & 252;
		goto l1;
	}
l1:	/* end sizeBitsOf: */;
	context = null;
	/* begin pushRemappableOop: */
	remapBuffer[remapBufferCount += 1] = methodContext;
	newContext = instantiateContextsizeInBytes(longAt(((((char *) specialObjectsOop)) + 4) + (11 << 2)), contextSize);
	/* begin popRemappableOop */
	oop = remapBuffer[remapBufferCount];
	remapBufferCount -= 1;
	methodContext = oop;
	initialIP = (((instructionPointer - method) << 1) | 1);
	longAtput(((((char *) newContext)) + 4) + (4 << 2), initialIP);
	longAtput(((((char *) newContext)) + 4) + (1 << 2), initialIP);
	/* begin storeStackPointerValue:inContext: */
	longAtput(((((char *) newContext)) + 4) + (2 << 2), ((0 << 1) | 1));
	longAtput(((((char *) newContext)) + 4) + (3 << 2), longAt(stackPointer - (0 * 4)));
	longAtput(((((char *) newContext)) + 4) + (5 << 2), methodContext);
	longAtput(((((char *) newContext)) + 4) + (0 << 2), nilObj);
	/* begin pop:thenPush: */
	longAtput(sp = stackPointer - ((2 - 1) * 4), newContext);
	stackPointer = sp;
}

void primitiveBytesLeft(void) {
    int integerValue;
    int sp;

	/* begin pop: */
	stackPointer -= 1 * 4;
	/* begin pushInteger: */
	integerValue = (longAt(freeBlock)) & 4294967292U;
	/* begin push: */
	longAtput(sp = stackPointer + 4, ((integerValue << 1) | 1));
	stackPointer = sp;
}

void primitiveClass(void) {
    int instance;
    int top;
    int object;
    int sp;
    int ccIndex;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	instance = top;
	/* begin push: */
	/* begin fetchClassOf: */
	if ((instance & 1)) {
		object = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l1;
	}
	ccIndex = (((unsigned) (longAt(instance))) >> 12) & 31;
	if (ccIndex == 0) {
		object = (longAt(instance - 4)) & 4294967292U;
		goto l1;
	} else {
		object = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l1;
	}
l1:	/* end fetchClassOf: */;
	longAtput(sp = stackPointer + 4, object);
	stackPointer = sp;
}

void primitiveClone(void) {
    int newCopy;
    int sp;

	newCopy = clone(longAt(stackPointer));
	/* begin pop:thenPush: */
	longAtput(sp = stackPointer - ((1 - 1) * 4), newCopy);
	stackPointer = sp;
}

void primitiveConstantFill(void) {
    int i;
    int end;
    int rcvrIsBytes;
    int fillValue;
    int rcvr;
    int successValue;
    int successValue1;
    int fmt;

	fillValue = positive32BitValueOf(longAt(stackPointer));
	rcvr = longAt(stackPointer - (1 * 4));
	/* begin success: */
	/* begin isWordsOrBytes: */
	fmt = (((unsigned) (longAt(rcvr))) >> 8) & 15;
	successValue1 = (fmt == 6) || ((fmt >= 8) && (fmt <= 11));
	successFlag = successValue1 && successFlag;
	rcvrIsBytes = ((((unsigned) (longAt(rcvr))) >> 8) & 15) >= 8;
	if (rcvrIsBytes) {
		/* begin success: */
		successValue = (fillValue >= 0) && (fillValue <= 255);
		successFlag = successValue && successFlag;
	}
	if (successFlag) {
		end = rcvr + (sizeBitsOf(rcvr));
		i = rcvr + 4;
		if (rcvrIsBytes) {
			while (i < end) {
				byteAtput(i, fillValue);
				i += 1;
			}
		} else {
			while (i < end) {
				longAtput(i, fillValue);
				i += 4;
			}
		}
		/* begin pop: */
		stackPointer -= 1 * 4;
	}
}

void primitiveCopyBits(void) {
    int rcvr;
    int successValue;

	rcvr = longAt(stackPointer - (argumentCount * 4));
	/* begin success: */
	successValue = loadBitBltFrom(rcvr);
	successFlag = successValue && successFlag;
	if (successFlag) {
		copyBits();
		showDisplayBits();
	}
}

void primitiveDeferDisplayUpdates(void) {
    int flag;

	flag = longAt(stackPointer);
	if (flag == trueObj) {
		deferDisplayUpdates = 1;
	} else {
		if (flag == falseObj) {
			deferDisplayUpdates = 0;
		} else {
			successFlag = 0;
		}
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 1 * 4;
	}
}

void primitiveDirectoryDelimitor(void) {
    int ascii;
    int sp;
    int successValue;

	ascii = asciiDirectoryDelimiter();
	/* begin success: */
	successValue = (ascii >= 0) && (ascii <= 255);
	successFlag = successValue && successFlag;
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 1 * 4;
		/* begin push: */
		longAtput(sp = stackPointer + 4, longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (24 << 2))))) + 4) + (ascii << 2)));
		stackPointer = sp;
	}
}

void primitiveDiv(void) {
    int quotient;
    int sp;

	quotient = doPrimitiveDivby(longAt(stackPointer - (1 * 4)), longAt(stackPointer - (0 * 4)));
	/* begin pop2AndPushIntegerIfOK: */
	if (successFlag) {
		if ((quotient ^ (quotient << 1)) >= 0) {
			/* begin pop:thenPush: */
			longAtput(sp = stackPointer - ((2 - 1) * 4), ((quotient << 1) | 1));
			stackPointer = sp;
		} else {
			successFlag = 0;
		}
	}
}

void primitiveDivide(void) {
    int integerReceiver;
    int integerArgument;
    int integerPointer;
    int integerPointer1;
    int sp;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (1 * 4));
	if ((integerPointer & 1)) {
		integerReceiver = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerReceiver = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (0 * 4));
	if ((integerPointer1 & 1)) {
		integerArgument = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		integerArgument = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	if ((integerArgument != 0) && ((integerReceiver % integerArgument) == 0)) {
		/* begin pop2AndPushIntegerIfOK: */
		if (successFlag) {
			if (((integerReceiver / integerArgument) ^ ((integerReceiver / integerArgument) << 1)) >= 0) {
				/* begin pop:thenPush: */
				longAtput(sp = stackPointer - ((2 - 1) * 4), (((integerReceiver / integerArgument) << 1) | 1));
				stackPointer = sp;
			} else {
				successFlag = 0;
			}
		}
	} else {
		successFlag = 0;
	}
}

int primitiveDoPrimitiveWithArgs(void) {
    int primIdx;
    int argumentArray;
    int arraySize;
    int index;
    int cntxSize;
    int sp;
    int sp1;
    int sp2;
    int sz;
    int header;
    int sz1;
    int header1;
    int integerPointer;
    int oop;

	argumentArray = longAt(stackPointer);
	/* begin fetchWordLengthOf: */
	/* begin sizeBitsOf: */
	header = longAt(argumentArray);
	if ((header & 3) == 0) {
		sz = (longAt(argumentArray - 8)) & 4294967292U;
		goto l1;
	} else {
		sz = header & 252;
		goto l1;
	}
l1:	/* end sizeBitsOf: */;
	arraySize = ((unsigned) (sz - 4)) >> 2;
	/* begin fetchWordLengthOf: */
	/* begin sizeBitsOf: */
	header1 = longAt(activeContext);
	if ((header1 & 3) == 0) {
		sz1 = (longAt(activeContext - 8)) & 4294967292U;
		goto l2;
	} else {
		sz1 = header1 & 252;
		goto l2;
	}
l2:	/* end sizeBitsOf: */;
	cntxSize = ((unsigned) (sz1 - 4)) >> 2;
	successFlag = (((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) + arraySize) < cntxSize) && successFlag;
	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (1 * 4));
	if ((integerPointer & 1)) {
		primIdx = (integerPointer >> 1);
		goto l3;
	} else {
		successFlag = 0;
		primIdx = 0;
		goto l3;
	}
l3:	/* end stackIntegerValue: */;
	if (!(successFlag)) {
		return successFlag = 0;
	}
	/* begin pop: */
	stackPointer -= 2 * 4;
	primitiveIndex = primIdx;
	argumentCount = arraySize;
	index = 1;
	while (index <= argumentCount) {
		/* begin push: */
		longAtput(sp = stackPointer + 4, longAt(((((char *) argumentArray)) + 4) + ((index - 1) << 2)));
		stackPointer = sp;
		index += 1;
	}
	/* begin pushRemappableOop: */
	remapBuffer[remapBufferCount += 1] = argumentArray;
	lkupClass = nilObj;
	primitiveResponse();
	/* begin popRemappableOop */
	oop = remapBuffer[remapBufferCount];
	remapBufferCount -= 1;
	argumentArray = oop;
	if (!(successFlag)) {
		/* begin pop: */
		stackPointer -= arraySize * 4;
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp1 = stackPointer + 4, ((primIdx << 1) | 1));
		stackPointer = sp1;
		/* begin push: */
		longAtput(sp2 = stackPointer + 4, argumentArray);
		stackPointer = sp2;
		argumentCount = 2;
	}
}

void primitiveDrawLoop(void) {
    int yDelta;
    int rcvr;
    int xDelta;
    int affL;
    int dx1;
    int dy1;
    int px;
    int py;
    int affR;
    int affT;
    int affB;
    int i;
    int P;
    int integerPointer;
    int integerPointer1;
    int successValue;

	rcvr = longAt(stackPointer - (2 * 4));
	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (1 * 4));
	if ((integerPointer & 1)) {
		xDelta = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		xDelta = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (0 * 4));
	if ((integerPointer1 & 1)) {
		yDelta = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		yDelta = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	/* begin success: */
	successValue = loadBitBltFrom(rcvr);
	successFlag = successValue && successFlag;
	if (successFlag) {
		/* begin drawLoopX:Y: */
		if (xDelta > 0) {
			dx1 = 1;
		} else {
			if (xDelta == 0) {
				dx1 = 0;
			} else {
				dx1 = -1;
			}
		}
		if (yDelta > 0) {
			dy1 = 1;
		} else {
			if (yDelta == 0) {
				dy1 = 0;
			} else {
				dy1 = -1;
			}
		}
		px = yDelta * dy1;
		py = xDelta * dx1;
		affL = affT = 9999;
		affR = affB = -9999;
		if (py > px) {
			P = ((int) py >> 1);
			for (i = 1; i <= py; i += 1) {
				destX += dx1;
				if ((P -= px) < 0) {
					destY += dy1;
					P += py;
				}
				if (i < py) {
					copyBits();
					if ((affectedL < affectedR) && (affectedT < affectedB)) {
						affL = ((affL < affectedL) ? affL : affectedL);
						affR = ((affR < affectedR) ? affectedR : affR);
						affT = ((affT < affectedT) ? affT : affectedT);
						affB = ((affB < affectedB) ? affectedB : affB);
						if (((affR - affL) * (affB - affT)) > 4000) {
							affectedL = affL;
							affectedR = affR;
							affectedT = affT;
							affectedB = affB;
							showDisplayBits();
							affL = affT = 9999;
							affR = affB = -9999;
						}
					}
				}
			}
		} else {
			P = ((int) px >> 1);
			for (i = 1; i <= px; i += 1) {
				destY += dy1;
				if ((P -= py) < 0) {
					destX += dx1;
					P += px;
				}
				if (i < px) {
					copyBits();
					if ((affectedL < affectedR) && (affectedT < affectedB)) {
						affL = ((affL < affectedL) ? affL : affectedL);
						affR = ((affR < affectedR) ? affectedR : affR);
						affT = ((affT < affectedT) ? affT : affectedT);
						affB = ((affB < affectedB) ? affectedB : affB);
						if (((affR - affL) * (affB - affT)) > 4000) {
							affectedL = affL;
							affectedR = affR;
							affectedT = affT;
							affectedB = affB;
							showDisplayBits();
							affL = affT = 9999;
							affR = affB = -9999;
						}
					}
				}
			}
		}
		affectedL = affL;
		affectedR = affR;
		affectedT = affT;
		affectedB = affB;
		/* begin storeInteger:ofObject:withValue: */
		if ((destX ^ (destX << 1)) >= 0) {
			longAtput(((((char *) bitBltOop)) + 4) + (4 << 2), ((destX << 1) | 1));
		} else {
			successFlag = 0;
		}
		/* begin storeInteger:ofObject:withValue: */
		if ((destY ^ (destY << 1)) >= 0) {
			longAtput(((((char *) bitBltOop)) + 4) + (5 << 2), ((destY << 1) | 1));
		} else {
			successFlag = 0;
		}
		showDisplayBits();
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
	}
}

void primitiveEqual(void) {
    int integerReceiver;
    int integerArgument;
    int result;
    int top;
    int top1;
    int sp;
    int sp1;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerArgument = top;
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerReceiver = top1;
	/* begin compare31or32Bits:equal: */
	if (((integerReceiver & 1)) && ((integerArgument & 1))) {
		result = integerReceiver == integerArgument;
		goto l1;
	}
	result = (positive32BitValueOf(integerReceiver)) == (positive32BitValueOf(integerArgument));
l1:	/* end compare31or32Bits:equal: */;
	/* begin checkBooleanResult: */
	if (successFlag) {
		/* begin pushBool: */
		if (result) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveEquivalent(void) {
    int thisObject;
    int otherObject;
    int top;
    int top1;
    int sp;
    int sp1;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	otherObject = top;
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	thisObject = top1;
	/* begin pushBool: */
	if (thisObject == otherObject) {
		/* begin push: */
		longAtput(sp = stackPointer + 4, trueObj);
		stackPointer = sp;
	} else {
		/* begin push: */
		longAtput(sp1 = stackPointer + 4, falseObj);
		stackPointer = sp1;
	}
}

void primitiveExitToDebugger(void) {
	error("Exit to debugger at user request");
	printCallStackFrom(activeContext);
}

void primitiveExp(void) {
    double rcvr;

	rcvr = popFloat();
	if (successFlag) {
		pushFloat(exp(rcvr));
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveExponent(void) {
    int pwr;
    double frac;
    double rcvr;
    int sp;

	rcvr = popFloat();
	if (successFlag) {
		frac = frexp(rcvr, &pwr);
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp = stackPointer + 4, (((pwr - 1) << 1) | 1));
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

int primitiveFail(void) {
	return successFlag = 0;
}

void primitiveFileAtEnd(void) {
    int atEnd;
    SQFile *file;
    int sp;
    int sp1;

	file = fileValueOf(longAt(stackPointer));
	if (successFlag) {
		atEnd = sqFileAtEnd(file);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin pushBool: */
		if (atEnd) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	}
}

void primitiveFileClose(void) {
    SQFile *file;

	file = fileValueOf(longAt(stackPointer));
	if (successFlag) {
		sqFileClose(file);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 1 * 4;
	}
}

void primitiveFileDelete(void) {
    int nameIndex;
    int namePointer;
    int nameSize;
    int header;
    int sz;

	namePointer = longAt(stackPointer);
	successFlag = (((((unsigned) (longAt(namePointer))) >> 8) & 15) >= 8) && successFlag;
	if (successFlag) {
		nameIndex = namePointer + 4;
		/* begin lengthOf: */
		header = longAt(namePointer);
		/* begin lengthOf:baseHeader:format: */
		if ((header & 3) == 0) {
			sz = (longAt(namePointer - 8)) & 4294967292U;
		} else {
			sz = header & 252;
		}
		if (((((unsigned) header) >> 8) & 15) < 8) {
			nameSize = ((unsigned) (sz - 4)) >> 2;
			goto l1;
		} else {
			nameSize = (sz - 4) - (((((unsigned) header) >> 8) & 15) & 3);
			goto l1;
		}
		nameSize = null;
	l1:	/* end lengthOf: */;
	}
	if (successFlag) {
		sqFileDeleteNameSize(nameIndex, nameSize);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 1 * 4;
	}
}

void primitiveFileGetPosition(void) {
    int position;
    SQFile *file;
    int oop;
    int sp;

	file = fileValueOf(longAt(stackPointer));
	if (successFlag) {
		position = sqFileGetPosition(file);
	}
	if (successFlag) {
		/* begin pop:thenPush: */
		oop = positive32BitIntegerFor(position);
		longAtput(sp = stackPointer - ((2 - 1) * 4), oop);
		stackPointer = sp;
	}
}

void primitiveFileOpen(void) {
    int writeFlag;
    int nameIndex;
    int namePointer;
    int nameSize;
    int filePointer;
    SQFile *file;
    int header;
    int sz;
    int sp;

	/* begin booleanValueOf: */
	if ((longAt(stackPointer)) == trueObj) {
		writeFlag = 1;
		goto l2;
	}
	if ((longAt(stackPointer)) == falseObj) {
		writeFlag = 0;
		goto l2;
	}
	successFlag = 0;
	writeFlag = null;
l2:	/* end booleanValueOf: */;
	namePointer = longAt(stackPointer - (1 * 4));
	successFlag = (((((unsigned) (longAt(namePointer))) >> 8) & 15) >= 8) && successFlag;
	if (successFlag) {
		filePointer = instantiateClassindexableSize(longAt(((((char *) specialObjectsOop)) + 4) + (26 << 2)), fileRecordSize());
		file = fileValueOf(filePointer);
		nameIndex = namePointer + 4;
		/* begin lengthOf: */
		header = longAt(namePointer);
		/* begin lengthOf:baseHeader:format: */
		if ((header & 3) == 0) {
			sz = (longAt(namePointer - 8)) & 4294967292U;
		} else {
			sz = header & 252;
		}
		if (((((unsigned) header) >> 8) & 15) < 8) {
			nameSize = ((unsigned) (sz - 4)) >> 2;
			goto l1;
		} else {
			nameSize = (sz - 4) - (((((unsigned) header) >> 8) & 15) & 3);
			goto l1;
		}
		nameSize = null;
	l1:	/* end lengthOf: */;
	}
	if (successFlag) {
		sqFileOpen(file, nameIndex, nameSize, writeFlag);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 3 * 4;
		/* begin push: */
		longAtput(sp = stackPointer + 4, filePointer);
		stackPointer = sp;
	}
}

void primitiveFileRead(void) {
    int array;
    int startIndex;
    int arrayIndex;
    int bytesRead;
    int byteSize;
    int count;
    SQFile *file;
    int sp;
    int integerPointer;
    int integerPointer1;
    int successValue;
    int successValue1;
    int fmt;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (0 * 4));
	if ((integerPointer & 1)) {
		count = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		count = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (1 * 4));
	if ((integerPointer1 & 1)) {
		startIndex = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		startIndex = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	array = longAt(stackPointer - (2 * 4));
	file = fileValueOf(longAt(stackPointer - (3 * 4)));
	/* begin success: */
	/* begin isWordsOrBytes: */
	fmt = (((unsigned) (longAt(array))) >> 8) & 15;
	successValue = (fmt == 6) || ((fmt >= 8) && (fmt <= 11));
	successFlag = successValue && successFlag;
	if (((((unsigned) (longAt(array))) >> 8) & 15) == 6) {
		byteSize = 4;
	} else {
		byteSize = 1;
	}
	/* begin success: */
	successValue1 = (startIndex >= 1) && (((startIndex + count) - 1) <= (lengthOf(array)));
	successFlag = successValue1 && successFlag;
	if (successFlag) {
		arrayIndex = array + 4;
		bytesRead = sqFileReadIntoAt(file, count * byteSize, arrayIndex, (startIndex - 1) * byteSize);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 5 * 4;
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp = stackPointer + 4, (((bytesRead / byteSize) << 1) | 1));
		stackPointer = sp;
	}
}

void primitiveFileRename(void) {
    int newNameIndex;
    int newNamePointer;
    int newNameSize;
    int oldNamePointer;
    int oldNameIndex;
    int oldNameSize;
    int header;
    int sz;
    int header1;
    int sz1;

	newNamePointer = longAt(stackPointer);
	oldNamePointer = longAt(stackPointer - (1 * 4));
	successFlag = (((((unsigned) (longAt(newNamePointer))) >> 8) & 15) >= 8) && successFlag;
	successFlag = (((((unsigned) (longAt(oldNamePointer))) >> 8) & 15) >= 8) && successFlag;
	if (successFlag) {
		newNameIndex = newNamePointer + 4;
		/* begin lengthOf: */
		header = longAt(newNamePointer);
		/* begin lengthOf:baseHeader:format: */
		if ((header & 3) == 0) {
			sz = (longAt(newNamePointer - 8)) & 4294967292U;
		} else {
			sz = header & 252;
		}
		if (((((unsigned) header) >> 8) & 15) < 8) {
			newNameSize = ((unsigned) (sz - 4)) >> 2;
			goto l1;
		} else {
			newNameSize = (sz - 4) - (((((unsigned) header) >> 8) & 15) & 3);
			goto l1;
		}
		newNameSize = null;
	l1:	/* end lengthOf: */;
		oldNameIndex = oldNamePointer + 4;
		/* begin lengthOf: */
		header1 = longAt(oldNamePointer);
		/* begin lengthOf:baseHeader:format: */
		if ((header1 & 3) == 0) {
			sz1 = (longAt(oldNamePointer - 8)) & 4294967292U;
		} else {
			sz1 = header1 & 252;
		}
		if (((((unsigned) header1) >> 8) & 15) < 8) {
			oldNameSize = ((unsigned) (sz1 - 4)) >> 2;
			goto l2;
		} else {
			oldNameSize = (sz1 - 4) - (((((unsigned) header1) >> 8) & 15) & 3);
			goto l2;
		}
		oldNameSize = null;
	l2:	/* end lengthOf: */;
	}
	if (successFlag) {
		sqFileRenameOldSizeNewSize(oldNameIndex, oldNameSize, newNameIndex, newNameSize);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
	}
}

void primitiveFileSetPosition(void) {
    int newPosition;
    SQFile *file;

	newPosition = positive32BitValueOf(longAt(stackPointer - (0 * 4)));
	file = fileValueOf(longAt(stackPointer - (1 * 4)));
	if (successFlag) {
		sqFileSetPosition(file, newPosition);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
	}
}

void primitiveFileSize(void) {
    int size;
    SQFile *file;
    int oop;
    int sp;

	file = fileValueOf(longAt(stackPointer));
	if (successFlag) {
		size = sqFileSize(file);
	}
	if (successFlag) {
		/* begin pop:thenPush: */
		oop = positive32BitIntegerFor(size);
		longAtput(sp = stackPointer - ((2 - 1) * 4), oop);
		stackPointer = sp;
	}
}

void primitiveFileWrite(void) {
    int array;
    int startIndex;
    int arrayIndex;
    int bytesWritten;
    int byteSize;
    int count;
    SQFile *file;
    int sp;
    int integerPointer;
    int integerPointer1;
    int successValue;
    int successValue1;
    int fmt;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (0 * 4));
	if ((integerPointer & 1)) {
		count = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		count = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (1 * 4));
	if ((integerPointer1 & 1)) {
		startIndex = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		startIndex = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	array = longAt(stackPointer - (2 * 4));
	file = fileValueOf(longAt(stackPointer - (3 * 4)));
	/* begin success: */
	/* begin isWordsOrBytes: */
	fmt = (((unsigned) (longAt(array))) >> 8) & 15;
	successValue = (fmt == 6) || ((fmt >= 8) && (fmt <= 11));
	successFlag = successValue && successFlag;
	if (((((unsigned) (longAt(array))) >> 8) & 15) == 6) {
		byteSize = 4;
	} else {
		byteSize = 1;
	}
	/* begin success: */
	successValue1 = (startIndex >= 1) && (((startIndex + count) - 1) <= (lengthOf(array)));
	successFlag = successValue1 && successFlag;
	if (successFlag) {
		arrayIndex = array + 4;
		bytesWritten = sqFileWriteFromAt(file, count * byteSize, arrayIndex, (startIndex - 1) * byteSize);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 5 * 4;
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp = stackPointer + 4, (((bytesWritten / byteSize) << 1) | 1));
		stackPointer = sp;
	}
}

void primitiveFloatAdd(void) {
	primitiveFloatAddtoArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
}

void primitiveFloatAddtoArg(int rcvrOop, int argOop) {
    double arg;
    double rcvr;
    int object;
    int sp;

	rcvr = loadFloatOrIntFrom(rcvrOop);
	arg = loadFloatOrIntFrom(argOop);
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin push: */
		object = floatObjectOf(rcvr + arg);
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	}
}

void primitiveFloatDivide(void) {
	primitiveFloatDividebyArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
}

void primitiveFloatDividebyArg(int rcvrOop, int argOop) {
    double arg;
    double rcvr;
    int object;
    int sp;

	rcvr = loadFloatOrIntFrom(rcvrOop);
	arg = loadFloatOrIntFrom(argOop);
	if (successFlag) {
		successFlag = (arg != 0.0) && successFlag;
		if (successFlag) {
			/* begin pop: */
			stackPointer -= 2 * 4;
			/* begin push: */
			object = floatObjectOf(rcvr / arg);
			longAtput(sp = stackPointer + 4, object);
			stackPointer = sp;
		}
	}
}

void primitiveFloatEqual(void) {
    int aBool;
    int sp;
    int sp1;

	aBool = primitiveFloatEqualtoArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin pushBool: */
		if (aBool) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	}
}

int primitiveFloatEqualtoArg(int rcvrOop, int argOop) {
    double arg;
    double rcvr;

	rcvr = loadFloatOrIntFrom(rcvrOop);
	arg = loadFloatOrIntFrom(argOop);
	if (successFlag) {
		return rcvr == arg;
	}
}

int primitiveFloatGreaterthanArg(int rcvrOop, int argOop) {
    double arg;
    double rcvr;

	rcvr = loadFloatOrIntFrom(rcvrOop);
	arg = loadFloatOrIntFrom(argOop);
	if (successFlag) {
		return rcvr > arg;
	}
}

void primitiveFloatGreaterOrEqual(void) {
    int aBool;
    int sp;
    int sp1;

	aBool = primitiveFloatLessthanArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin pushBool: */
		if (!aBool) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	}
}

void primitiveFloatGreaterThan(void) {
    int aBool;
    int sp;
    int sp1;

	aBool = primitiveFloatGreaterthanArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin pushBool: */
		if (aBool) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	}
}

int primitiveFloatLessthanArg(int rcvrOop, int argOop) {
    double arg;
    double rcvr;

	rcvr = loadFloatOrIntFrom(rcvrOop);
	arg = loadFloatOrIntFrom(argOop);
	if (successFlag) {
		return rcvr < arg;
	}
}

void primitiveFloatLessOrEqual(void) {
    int aBool;
    int sp;
    int sp1;

	aBool = primitiveFloatGreaterthanArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin pushBool: */
		if (!aBool) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	}
}

void primitiveFloatLessThan(void) {
    int aBool;
    int sp;
    int sp1;

	aBool = primitiveFloatLessthanArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin pushBool: */
		if (aBool) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	}
}

void primitiveFloatMultiply(void) {
	primitiveFloatMultiplybyArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
}

void primitiveFloatMultiplybyArg(int rcvrOop, int argOop) {
    double arg;
    double rcvr;
    int object;
    int sp;

	rcvr = loadFloatOrIntFrom(rcvrOop);
	arg = loadFloatOrIntFrom(argOop);
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin push: */
		object = floatObjectOf(rcvr * arg);
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	}
}

void primitiveFloatNotEqual(void) {
    int aBool;
    int sp;
    int sp1;

	aBool = primitiveFloatEqualtoArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin pushBool: */
		if (!aBool) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	}
}

void primitiveFloatSubtract(void) {
	primitiveFloatSubtractfromArg(longAt(stackPointer - (1 * 4)), longAt(stackPointer));
}

void primitiveFloatSubtractfromArg(int rcvrOop, int argOop) {
    double arg;
    double rcvr;
    int object;
    int sp;

	rcvr = loadFloatOrIntFrom(rcvrOop);
	arg = loadFloatOrIntFrom(argOop);
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin push: */
		object = floatObjectOf(rcvr - arg);
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	}
}

void primitiveFlushCache(void) {
    int i;

	/* begin flushMethodCache */
	for (i = 1; i <= 1024; i += 1) {
		methodCache[i] = 0;
	}
	for (i = 1; i <= 64; i += 1) {
		atCache[i] = 0;
	}
}

void primitiveFlushCacheByMethod(void) {
    int probe;
    int i;
    int oldMethod;

	oldMethod = longAt(stackPointer);
	probe = 0;
	for (i = 1; i <= 256; i += 1) {
		if ((methodCache[probe + 3]) == oldMethod) {
			methodCache[probe + 1] = 0;
		}
		probe += 4;
	}
}

void primitiveFlushCacheSelective(void) {
    int probe;
    int selector;
    int i;

	selector = longAt(stackPointer);
	probe = 0;
	for (i = 1; i <= 256; i += 1) {
		if ((methodCache[probe + 1]) == selector) {
			methodCache[probe + 1] = 0;
		}
		probe += 4;
	}
}

void primitiveFractionalPart(void) {
    double trunc;
    double frac;
    double rcvr;

	rcvr = popFloat();
	if (successFlag) {
		frac = modf(rcvr, &trunc);
		pushFloat(frac);
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveFullGC(void) {
    int sp;

	/* begin pop: */
	stackPointer -= 1 * 4;
	incrementalGC();
	fullGC();
	/* begin pushInteger: */
	/* begin push: */
	longAtput(sp = stackPointer + 4, ((((longAt(freeBlock)) & 4294967292U) << 1) | 1));
	stackPointer = sp;
}

void primitiveGetAttribute(void) {
    int attr;
    int sz;
    int s;
    int sp;
    int integerPointer;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (0 * 4));
	if ((integerPointer & 1)) {
		attr = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		attr = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	if (successFlag) {
		sz = attributeSize(attr);
	}
	if (successFlag) {
		s = instantiateClassindexableSize(longAt(((((char *) specialObjectsOop)) + 4) + (6 << 2)), sz);
		getAttributeIntoLength(attr, s + 4, sz);
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin push: */
		longAtput(sp = stackPointer + 4, s);
		stackPointer = sp;
	}
}

void primitiveGreaterOrEqual(void) {
    int integerReceiver;
    int integerArgument;
    int integerPointer;
    int integerPointer1;
    int top;
    int top1;
    int sp;
    int sp1;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		integerArgument = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerArgument = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer1 = top1;
	if ((integerPointer1 & 1)) {
		integerReceiver = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		integerReceiver = 1;
		goto l2;
	}
l2:	/* end popInteger */;
	/* begin checkBooleanResult: */
	if (successFlag) {
		/* begin pushBool: */
		if (integerReceiver >= integerArgument) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveGreaterThan(void) {
    int integerReceiver;
    int integerArgument;
    int integerPointer;
    int integerPointer1;
    int top;
    int top1;
    int sp;
    int sp1;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		integerArgument = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerArgument = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer1 = top1;
	if ((integerPointer1 & 1)) {
		integerReceiver = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		integerReceiver = 1;
		goto l2;
	}
l2:	/* end popInteger */;
	/* begin checkBooleanResult: */
	if (successFlag) {
		/* begin pushBool: */
		if (integerReceiver > integerArgument) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveImageName(void) {
    int sz;
    int s;
    int sp;

	sz = imageNameSize();
	s = instantiateClassindexableSize(longAt(((((char *) specialObjectsOop)) + 4) + (6 << 2)), sz);
	imageNameGetLength(s + 4, sz);
	/* begin pop: */
	stackPointer -= 1 * 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, s);
	stackPointer = sp;
}

void primitiveIncrementalGC(void) {
    int sp;

	/* begin pop: */
	stackPointer -= 1 * 4;
	incrementalGC();
	/* begin pushInteger: */
	/* begin push: */
	longAtput(sp = stackPointer + 4, ((((longAt(freeBlock)) & 4294967292U) << 1) | 1));
	stackPointer = sp;
}

int primitiveIndexOf(int methodPointer) {
    int primBits;

	primBits = (((unsigned) (longAt(((((char *) methodPointer)) + 4) + (0 << 2)))) >> 1) & 805306879;
	if (primBits > 511) {
		return (primBits & 511) + (((unsigned) primBits) >> 19);
	} else {
		return primBits;
	}
}

void primitiveInstVarAt(void) {
    int value;
    int hdr;
    int totalLength;
    int index;
    int fmt;
    int rcvr;
    int fixedFields;
    int sz;
    int sp;
    int integerPointer;
    int top;
    int top1;
    int classFormat;
    int cls;
    int ccIndex;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		index = (integerPointer >> 1);
		goto l3;
	} else {
		successFlag = 0;
		index = 1;
		goto l3;
	}
l3:	/* end popInteger */;
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	rcvr = top1;
	if (successFlag) {
		hdr = longAt(rcvr);
		fmt = (((unsigned) hdr) >> 8) & 15;
		/* begin lengthOf:baseHeader:format: */
		if ((hdr & 3) == 0) {
			sz = (longAt(rcvr - 8)) & 4294967292U;
		} else {
			sz = hdr & 252;
		}
		if (fmt < 8) {
			totalLength = ((unsigned) (sz - 4)) >> 2;
			goto l1;
		} else {
			totalLength = (sz - 4) - (fmt & 3);
			goto l1;
		}
	l1:	/* end lengthOf:baseHeader:format: */;
		/* begin fixedFieldsOf:format:length: */
		if ((fmt > 4) || (fmt == 2)) {
			fixedFields = 0;
			goto l4;
		}
		if (fmt < 2) {
			fixedFields = totalLength;
			goto l4;
		}
		/* begin fetchClassOf: */
		if ((rcvr & 1)) {
			cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l5;
		}
		ccIndex = (((unsigned) (longAt(rcvr))) >> 12) & 31;
		if (ccIndex == 0) {
			cls = (longAt(rcvr - 4)) & 4294967292U;
			goto l5;
		} else {
			cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l5;
		}
	l5:	/* end fetchClassOf: */;
		classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
		fixedFields = (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
	l4:	/* end fixedFieldsOf:format:length: */;
		if (!((index >= 1) && (index <= fixedFields))) {
			successFlag = 0;
		}
	}
	if (successFlag) {
		/* begin subscript:with:format: */
		if (fmt <= 4) {
			value = longAt(((((char *) rcvr)) + 4) + ((index - 1) << 2));
			goto l2;
		}
		if (fmt < 8) {
			value = positive32BitIntegerFor(longAt(((((char *) rcvr)) + 4) + ((index - 1) << 2)));
			goto l2;
		} else {
			value = (((byteAt(((((char *) rcvr)) + 4) + (index - 1))) << 1) | 1);
			goto l2;
		}
	l2:	/* end subscript:with:format: */;
	}
	if (successFlag) {
		/* begin push: */
		longAtput(sp = stackPointer + 4, value);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveInstVarAtPut(void) {
    int hdr;
    int newValue;
    int totalLength;
    int index;
    int fmt;
    int rcvr;
    int fixedFields;
    int sp;
    int top;
    int integerPointer;
    int top1;
    int top2;
    int sz;
    int classFormat;
    int cls;
    int valueToStore;
    int ccIndex;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	newValue = top;
	/* begin popInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top1;
	if ((integerPointer & 1)) {
		index = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		index = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popStack */
	top2 = longAt(stackPointer);
	stackPointer -= 4;
	rcvr = top2;
	if (successFlag) {
		hdr = longAt(rcvr);
		fmt = (((unsigned) hdr) >> 8) & 15;
		/* begin lengthOf:baseHeader:format: */
		if ((hdr & 3) == 0) {
			sz = (longAt(rcvr - 8)) & 4294967292U;
		} else {
			sz = hdr & 252;
		}
		if (fmt < 8) {
			totalLength = ((unsigned) (sz - 4)) >> 2;
			goto l2;
		} else {
			totalLength = (sz - 4) - (fmt & 3);
			goto l2;
		}
	l2:	/* end lengthOf:baseHeader:format: */;
		/* begin fixedFieldsOf:format:length: */
		if ((fmt > 4) || (fmt == 2)) {
			fixedFields = 0;
			goto l3;
		}
		if (fmt < 2) {
			fixedFields = totalLength;
			goto l3;
		}
		/* begin fetchClassOf: */
		if ((rcvr & 1)) {
			cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l4;
		}
		ccIndex = (((unsigned) (longAt(rcvr))) >> 12) & 31;
		if (ccIndex == 0) {
			cls = (longAt(rcvr - 4)) & 4294967292U;
			goto l4;
		} else {
			cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l4;
		}
	l4:	/* end fetchClassOf: */;
		classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
		fixedFields = (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
	l3:	/* end fixedFieldsOf:format:length: */;
		if (!((index >= 1) && (index <= fixedFields))) {
			successFlag = 0;
		}
	}
	if (successFlag) {
		/* begin subscript:with:storing:format: */
		if (fmt <= 4) {
			/* begin storePointer:ofObject:withValue: */
			if (rcvr < youngStart) {
				possibleRootStoreIntovalue(rcvr, newValue);
			}
			longAtput(((((char *) rcvr)) + 4) + ((index - 1) << 2), newValue);
		} else {
			if (fmt < 8) {
				valueToStore = positive32BitValueOf(newValue);
				if (successFlag) {
					longAtput(((((char *) rcvr)) + 4) + ((index - 1) << 2), valueToStore);
				}
			} else {
				if (!((newValue & 1))) {
					successFlag = 0;
				}
				valueToStore = (newValue >> 1);
				if (!((valueToStore >= 0) && (valueToStore <= 255))) {
					successFlag = 0;
				}
				if (successFlag) {
					byteAtput(((((char *) rcvr)) + 4) + (index - 1), valueToStore);
				}
			}
		}
	}
	if (successFlag) {
		/* begin push: */
		longAtput(sp = stackPointer + 4, newValue);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 3 * 4;
	}
}

void primitiveInterruptSemaphore(void) {
    int arg;
    int oop;
    int oop1;
    int valuePointer;
    int top;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	arg = top;
	if ((fetchClassOf(arg)) == (longAt(((((char *) specialObjectsOop)) + 4) + (18 << 2)))) {
		/* begin storePointer:ofObject:withValue: */
		oop = specialObjectsOop;
		if (oop < youngStart) {
			possibleRootStoreIntovalue(oop, arg);
		}
		longAtput(((((char *) oop)) + 4) + (30 << 2), arg);
	} else {
		/* begin storePointer:ofObject:withValue: */
		oop1 = specialObjectsOop;
		valuePointer = nilObj;
		if (oop1 < youngStart) {
			possibleRootStoreIntovalue(oop1, valuePointer);
		}
		longAtput(((((char *) oop1)) + 4) + (30 << 2), valuePointer);
	}
}

void primitiveKbdNext(void) {
    int keystrokeWord;
    int sp;
    int sp1;

	/* begin pop: */
	stackPointer -= 1 * 4;
	keystrokeWord = ioGetKeystroke();
	if (keystrokeWord >= 0) {
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp1 = stackPointer + 4, ((keystrokeWord << 1) | 1));
		stackPointer = sp1;
	} else {
		/* begin push: */
		longAtput(sp = stackPointer + 4, nilObj);
		stackPointer = sp;
	}
}

void primitiveKbdPeek(void) {
    int keystrokeWord;
    int sp;
    int sp1;

	/* begin pop: */
	stackPointer -= 1 * 4;
	keystrokeWord = ioPeekKeystroke();
	if (keystrokeWord >= 0) {
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp1 = stackPointer + 4, ((keystrokeWord << 1) | 1));
		stackPointer = sp1;
	} else {
		/* begin push: */
		longAtput(sp = stackPointer + 4, nilObj);
		stackPointer = sp;
	}
}

void primitiveLessOrEqual(void) {
    int integerReceiver;
    int integerArgument;
    int integerPointer;
    int integerPointer1;
    int top;
    int top1;
    int sp;
    int sp1;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		integerArgument = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerArgument = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer1 = top1;
	if ((integerPointer1 & 1)) {
		integerReceiver = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		integerReceiver = 1;
		goto l2;
	}
l2:	/* end popInteger */;
	/* begin checkBooleanResult: */
	if (successFlag) {
		/* begin pushBool: */
		if (integerReceiver <= integerArgument) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveLessThan(void) {
    int integerReceiver;
    int integerArgument;
    int integerPointer;
    int integerPointer1;
    int top;
    int top1;
    int sp;
    int sp1;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		integerArgument = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerArgument = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer1 = top1;
	if ((integerPointer1 & 1)) {
		integerReceiver = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		integerReceiver = 1;
		goto l2;
	}
l2:	/* end popInteger */;
	/* begin checkBooleanResult: */
	if (successFlag) {
		/* begin pushBool: */
		if (integerReceiver < integerArgument) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveLoadInstVar(void) {
    int thisReceiver;
    int top;
    int sp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	thisReceiver = top;
	/* begin push: */
	longAtput(sp = stackPointer + 4, longAt(((((char *) thisReceiver)) + 4) + ((primitiveIndex - 264) << 2)));
	stackPointer = sp;
}

void primitiveLogN(void) {
    double rcvr;

	rcvr = popFloat();
	if (successFlag) {
		pushFloat(log(rcvr));
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveLowSpaceSemaphore(void) {
    int arg;
    int oop;
    int oop1;
    int valuePointer;
    int top;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	arg = top;
	if ((fetchClassOf(arg)) == (longAt(((((char *) specialObjectsOop)) + 4) + (18 << 2)))) {
		/* begin storePointer:ofObject:withValue: */
		oop = specialObjectsOop;
		if (oop < youngStart) {
			possibleRootStoreIntovalue(oop, arg);
		}
		longAtput(((((char *) oop)) + 4) + (17 << 2), arg);
	} else {
		/* begin storePointer:ofObject:withValue: */
		oop1 = specialObjectsOop;
		valuePointer = nilObj;
		if (oop1 < youngStart) {
			possibleRootStoreIntovalue(oop1, valuePointer);
		}
		longAtput(((((char *) oop1)) + 4) + (17 << 2), valuePointer);
	}
}

void primitiveMakePoint(void) {
    int integerReceiver;
    int integerArgument;
    int integerPointer;
    int integerPointer1;
    int object;
    int sp;
    int top;
    int top1;
    int pointResult;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		integerArgument = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerArgument = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer1 = top1;
	if ((integerPointer1 & 1)) {
		integerReceiver = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		integerReceiver = 1;
		goto l2;
	}
l2:	/* end popInteger */;
	if (successFlag) {
		/* begin push: */
		/* begin makePointwithxValue:yValue: */
		pointResult = instantiateSmallClasssizeInBytesfill(longAt(((((char *) specialObjectsOop)) + 4) + (12 << 2)), 12, nilObj);
		/* begin storePointer:ofObject:withValue: */
		if (pointResult < youngStart) {
			possibleRootStoreIntovalue(pointResult, ((integerReceiver << 1) | 1));
		}
		longAtput(((((char *) pointResult)) + 4) + (0 << 2), ((integerReceiver << 1) | 1));
		/* begin storePointer:ofObject:withValue: */
		if (pointResult < youngStart) {
			possibleRootStoreIntovalue(pointResult, ((integerArgument << 1) | 1));
		}
		longAtput(((((char *) pointResult)) + 4) + (1 << 2), ((integerArgument << 1) | 1));
		object = pointResult;
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveMillisecondClock(void) {
    int object;
    int sp;

	/* begin pop: */
	stackPointer -= 1 * 4;
	/* begin push: */
	object = ((((ioMSecs()) & 536870911) << 1) | 1);
	longAtput(sp = stackPointer + 4, object);
	stackPointer = sp;
}

void primitiveMod(void) {
    int mod;
    int sp;

	mod = doPrimitiveModby(longAt(stackPointer - (1 * 4)), longAt(stackPointer - (0 * 4)));
	/* begin pop2AndPushIntegerIfOK: */
	if (successFlag) {
		if ((mod ^ (mod << 1)) >= 0) {
			/* begin pop:thenPush: */
			longAtput(sp = stackPointer - ((2 - 1) * 4), ((mod << 1) | 1));
			stackPointer = sp;
		} else {
			successFlag = 0;
		}
	}
}

void primitiveMouseButtons(void) {
    int buttonWord;
    int sp;

	/* begin pop: */
	stackPointer -= 1 * 4;
	buttonWord = ioGetButtonState();
	/* begin pushInteger: */
	/* begin push: */
	longAtput(sp = stackPointer + 4, ((buttonWord << 1) | 1));
	stackPointer = sp;
}

void primitiveMousePoint(void) {
    int y;
    int pointWord;
    int x;
    int object;
    int sp;
    int pointResult;

	/* begin pop: */
	stackPointer -= 1 * 4;
	pointWord = ioMousePoint();
	/* begin signExtend16: */
	if ((((((unsigned) pointWord) >> 16) & 65535) & 32768) == 0) {
		x = (((unsigned) pointWord) >> 16) & 65535;
		goto l1;
	} else {
		x = ((((unsigned) pointWord) >> 16) & 65535) - 65536;
		goto l1;
	}
l1:	/* end signExtend16: */;
	/* begin signExtend16: */
	if (((pointWord & 65535) & 32768) == 0) {
		y = pointWord & 65535;
		goto l2;
	} else {
		y = (pointWord & 65535) - 65536;
		goto l2;
	}
l2:	/* end signExtend16: */;
	/* begin push: */
	/* begin makePointwithxValue:yValue: */
	pointResult = instantiateSmallClasssizeInBytesfill(longAt(((((char *) specialObjectsOop)) + 4) + (12 << 2)), 12, nilObj);
	/* begin storePointer:ofObject:withValue: */
	if (pointResult < youngStart) {
		possibleRootStoreIntovalue(pointResult, ((x << 1) | 1));
	}
	longAtput(((((char *) pointResult)) + 4) + (0 << 2), ((x << 1) | 1));
	/* begin storePointer:ofObject:withValue: */
	if (pointResult < youngStart) {
		possibleRootStoreIntovalue(pointResult, ((y << 1) | 1));
	}
	longAtput(((((char *) pointResult)) + 4) + (1 << 2), ((y << 1) | 1));
	object = pointResult;
	longAtput(sp = stackPointer + 4, object);
	stackPointer = sp;
}

void primitiveMultiply(void) {
    int integerResult;
    int integerRcvr;
    int integerArg;
    int integerPointer;
    int integerPointer1;
    int sp;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (1 * 4));
	if ((integerPointer & 1)) {
		integerRcvr = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerRcvr = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (0 * 4));
	if ((integerPointer1 & 1)) {
		integerArg = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		integerArg = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	if (successFlag) {
		integerResult = integerRcvr * integerArg;
		if ((integerArg == 0) || ((integerResult / integerArg) == integerRcvr)) {
			/* begin pop2AndPushIntegerIfOK: */
			if (successFlag) {
				if ((integerResult ^ (integerResult << 1)) >= 0) {
					/* begin pop:thenPush: */
					longAtput(sp = stackPointer - ((2 - 1) * 4), ((integerResult << 1) | 1));
					stackPointer = sp;
				} else {
					successFlag = 0;
				}
			}
		} else {
			successFlag = 0;
		}
	}
}

void primitiveNew(void) {
    int spaceOkay;
    int cls;
    int object;
    int sp;
    int top;
    int okay;
    int format;
    int minFree;
    int minFree1;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	cls = top;
	/* begin sufficientSpaceToInstantiate:indexableSize: */
	format = (((unsigned) ((longAt(((((char *) cls)) + 4) + (2 << 2))) - 1)) >> 8) & 15;
	if (((((unsigned ) 0)) > 0) && (format < 2)) {
		spaceOkay = 0;
		goto l3;
	}
	if (format < 8) {
		/* begin sufficientSpaceToAllocate: */
		minFree = (lowSpaceThreshold + (2500 + (0 * 4))) + 4;
		if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
			okay = 1;
			goto l1;
		} else {
			okay = sufficientSpaceAfterGC(minFree);
			goto l1;
		}
	l1:	/* end sufficientSpaceToAllocate: */;
	} else {
		/* begin sufficientSpaceToAllocate: */
		minFree1 = (lowSpaceThreshold + (2500 + 0)) + 4;
		if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree1))) {
			okay = 1;
			goto l2;
		} else {
			okay = sufficientSpaceAfterGC(minFree1);
			goto l2;
		}
	l2:	/* end sufficientSpaceToAllocate: */;
	}
	spaceOkay = okay;
l3:	/* end sufficientSpaceToInstantiate:indexableSize: */;
	successFlag = spaceOkay && successFlag;
	if (successFlag) {
		/* begin push: */
		object = instantiateClassindexableSize(cls, 0);
		longAtput(sp = stackPointer + 4, object);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveNewMethod(void) {
    int size;
    int i;
    int header;
    int theMethod;
    int bytecodeCount;
    int cls;
    int literalCount;
    int valuePointer;
    int top;
    int integerPointer;
    int top1;
    int sp;
    int top2;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	header = top;
	/* begin popInteger */
	/* begin popStack */
	top2 = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top2;
	if ((integerPointer & 1)) {
		bytecodeCount = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		bytecodeCount = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	successFlag = ((header & 1)) && successFlag;
	if (!(successFlag)) {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	cls = top1;
	size = ((((((unsigned) header) >> 10) & 255) + 1) * 4) + bytecodeCount;
	theMethod = instantiateClassindexableSize(cls, size);
	/* begin storePointer:ofObject:withValue: */
	if (theMethod < youngStart) {
		possibleRootStoreIntovalue(theMethod, header);
	}
	longAtput(((((char *) theMethod)) + 4) + (0 << 2), header);
	literalCount = (((unsigned) header) >> 10) & 255;
	for (i = 1; i <= literalCount; i += 1) {
		/* begin storePointer:ofObject:withValue: */
		valuePointer = nilObj;
		if (theMethod < youngStart) {
			possibleRootStoreIntovalue(theMethod, valuePointer);
		}
		longAtput(((((char *) theMethod)) + 4) + (i << 2), valuePointer);
	}
	/* begin push: */
	longAtput(sp = stackPointer + 4, theMethod);
	stackPointer = sp;
}

void primitiveNewWithArg(void) {
    int spaceOkay;
    int size;
    int cls;
    int oop;
    int sp;
    int okay;
    int format;
    int minFree;
    int minFree1;

	size = positive32BitValueOf(longAt(stackPointer - (0 * 4)));
	cls = longAt(stackPointer - (1 * 4));
	successFlag = (size >= 0) && successFlag;
	if (successFlag) {
		/* begin sufficientSpaceToInstantiate:indexableSize: */
		format = (((unsigned) ((longAt(((((char *) cls)) + 4) + (2 << 2))) - 1)) >> 8) & 15;
		if (((((unsigned ) size)) > 0) && (format < 2)) {
			spaceOkay = 0;
			goto l3;
		}
		if (format < 8) {
			/* begin sufficientSpaceToAllocate: */
			minFree = (lowSpaceThreshold + (2500 + (size * 4))) + 4;
			if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
				okay = 1;
				goto l1;
			} else {
				okay = sufficientSpaceAfterGC(minFree);
				goto l1;
			}
		l1:	/* end sufficientSpaceToAllocate: */;
		} else {
			/* begin sufficientSpaceToAllocate: */
			minFree1 = (lowSpaceThreshold + (2500 + size)) + 4;
			if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree1))) {
				okay = 1;
				goto l2;
			} else {
				okay = sufficientSpaceAfterGC(minFree1);
				goto l2;
			}
		l2:	/* end sufficientSpaceToAllocate: */;
		}
		spaceOkay = okay;
	l3:	/* end sufficientSpaceToInstantiate:indexableSize: */;
		successFlag = spaceOkay && successFlag;
	}
	if (successFlag) {
		/* begin pop:thenPush: */
		oop = instantiateClassindexableSize(cls, size);
		longAtput(sp = stackPointer - ((2 - 1) * 4), oop);
		stackPointer = sp;
	}
}

int primitiveNext(void) {
    int array;
    int stream;
    int index;
    int limit;
    int result;
    int atIx;
    int sp;

	stream = longAt(stackPointer);
	if (!((((((unsigned) (longAt(stream))) >> 8) & 15) <= 4) && ((lengthOf(stream)) >= (2 + 1)))) {
		return successFlag = 0;
	}
	array = longAt(((((char *) stream)) + 4) + (0 << 2));
	index = fetchIntegerofObject(1, stream);
	limit = fetchIntegerofObject(2, stream);
	atIx = array & 28;
	if (!((index < limit) && ((atCache[atIx + 1]) == array))) {
		return successFlag = 0;
	}
	index += 1;
	result = commonVariableatcacheIndex(array, index, atIx);
	if (successFlag) {
		stream = longAt(stackPointer);
		/* begin storeInteger:ofObject:withValue: */
		if ((index ^ (index << 1)) >= 0) {
			longAtput(((((char *) stream)) + 4) + (1 << 2), ((index << 1) | 1));
		} else {
			successFlag = 0;
		}
		/* begin pop:thenPush: */
		longAtput(sp = stackPointer - ((1 - 1) * 4), result);
		return stackPointer = sp;
	}
}

void primitiveNextInstance(void) {
    int object;
    int instance;
    int sp;
    int top;
    int thisClass;
    int classPointer;
    int thisObj;
    int ccIndex;
    int ccIndex1;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	object = top;
	/* begin instanceAfter: */
	/* begin fetchClassOf: */
	if ((object & 1)) {
		classPointer = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l3;
	}
	ccIndex1 = (((unsigned) (longAt(object))) >> 12) & 31;
	if (ccIndex1 == 0) {
		classPointer = (longAt(object - 4)) & 4294967292U;
		goto l3;
	} else {
		classPointer = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex1 - 1) << 2));
		goto l3;
	}
l3:	/* end fetchClassOf: */;
	thisObj = accessibleObjectAfter(object);
	while (!(thisObj == null)) {
		/* begin fetchClassOf: */
		if ((thisObj & 1)) {
			thisClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l2;
		}
		ccIndex = (((unsigned) (longAt(thisObj))) >> 12) & 31;
		if (ccIndex == 0) {
			thisClass = (longAt(thisObj - 4)) & 4294967292U;
			goto l2;
		} else {
			thisClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l2;
		}
	l2:	/* end fetchClassOf: */;
		if (thisClass == classPointer) {
			instance = thisObj;
			goto l1;
		}
		thisObj = accessibleObjectAfter(thisObj);
	}
	instance = nilObj;
l1:	/* end instanceAfter: */;
	if (instance == nilObj) {
		/* begin unPop: */
		stackPointer += 1 * 4;
		successFlag = 0;
	} else {
		/* begin push: */
		longAtput(sp = stackPointer + 4, instance);
		stackPointer = sp;
	}
}

void primitiveNextObject(void) {
    int object;
    int instance;
    int sp;
    int top;
    int sp1;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	object = top;
	instance = accessibleObjectAfter(object);
	if (instance == null) {
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp1 = stackPointer + 4, ((0 << 1) | 1));
		stackPointer = sp1;
	} else {
		/* begin push: */
		longAtput(sp = stackPointer + 4, instance);
		stackPointer = sp;
	}
}

int primitiveNextPut(void) {
    int array;
    int stream;
    int value;
    int index;
    int limit;
    int atIx;
    int sp;
    int stSize;
    int valToPut;
    int fmt;
    int fixedFields;

	value = longAt(stackPointer);
	stream = longAt(stackPointer - (1 * 4));
	if (!((((((unsigned) (longAt(stream))) >> 8) & 15) <= 4) && ((lengthOf(stream)) >= (2 + 1)))) {
		return successFlag = 0;
	}
	array = longAt(((((char *) stream)) + 4) + (0 << 2));
	index = fetchIntegerofObject(1, stream);
	limit = fetchIntegerofObject(2, stream);
	atIx = (array & 28) + 32;
	if (!((index < limit) && ((atCache[atIx + 1]) == array))) {
		return successFlag = 0;
	}
	index += 1;
	/* begin commonVariable:at:put:cacheIndex: */
	stSize = atCache[atIx + 2];
	if (((((unsigned ) index)) >= 1) && ((((unsigned ) index)) <= (((unsigned ) stSize)))) {
		fmt = atCache[atIx + 3];
		if (fmt <= 4) {
			fixedFields = atCache[atIx + 4];
			/* begin storePointer:ofObject:withValue: */
			if (array < youngStart) {
				possibleRootStoreIntovalue(array, value);
			}
			longAtput(((((char *) array)) + 4) + (((index + fixedFields) - 1) << 2), value);
			goto l1;
		}
		if (fmt < 8) {
			valToPut = positive32BitValueOf(value);
			if (successFlag) {
				longAtput(((((char *) array)) + 4) + ((index - 1) << 2), valToPut);
			}
			goto l1;
		}
		if (fmt >= 16) {
			valToPut = asciiOfCharacter(value);
			if (!(successFlag)) {
				goto l1;
			}
		} else {
			valToPut = value;
		}
		if ((valToPut & 1)) {
			byteAtput(((((char *) array)) + 4) + (index - 1), (valToPut >> 1));
			goto l1;
		}
	}
	successFlag = 0;
l1:	/* end commonVariable:at:put:cacheIndex: */;
	if (successFlag) {
		/* begin storeInteger:ofObject:withValue: */
		if ((index ^ (index << 1)) >= 0) {
			longAtput(((((char *) stream)) + 4) + (1 << 2), ((index << 1) | 1));
		} else {
			successFlag = 0;
		}
		/* begin pop:thenPush: */
		longAtput(sp = stackPointer - ((2 - 1) * 4), value);
		return stackPointer = sp;
	}
}

void primitiveNotEqual(void) {
    int integerReceiver;
    int integerArgument;
    int result;
    int top;
    int top1;
    int sp;
    int sp1;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerArgument = top;
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerReceiver = top1;
	result = !(compare31or32Bitsequal(integerReceiver, integerArgument));
	/* begin checkBooleanResult: */
	if (successFlag) {
		/* begin pushBool: */
		if (result) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, trueObj);
			stackPointer = sp;
		} else {
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, falseObj);
			stackPointer = sp1;
		}
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveObjectAt(void) {
    int thisReceiver;
    int index;
    int sp;
    int integerPointer;
    int top;
    int top1;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		index = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		index = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	thisReceiver = top1;
	successFlag = (index > 0) && successFlag;
	successFlag = (index <= (((((unsigned) (longAt(((((char *) thisReceiver)) + 4) + (0 << 2)))) >> 10) & 255) + 1)) && successFlag;
	if (successFlag) {
		/* begin push: */
		longAtput(sp = stackPointer + 4, longAt(((((char *) thisReceiver)) + 4) + ((index - 1) << 2)));
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveObjectAtPut(void) {
    int thisReceiver;
    int newValue;
    int index;
    int sp;
    int top;
    int integerPointer;
    int top1;
    int top2;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	newValue = top;
	/* begin popInteger */
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top1;
	if ((integerPointer & 1)) {
		index = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		index = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popStack */
	top2 = longAt(stackPointer);
	stackPointer -= 4;
	thisReceiver = top2;
	successFlag = (index > 0) && successFlag;
	successFlag = (index <= (((((unsigned) (longAt(((((char *) thisReceiver)) + 4) + (0 << 2)))) >> 10) & 255) + 1)) && successFlag;
	if (successFlag) {
		/* begin storePointer:ofObject:withValue: */
		if (thisReceiver < youngStart) {
			possibleRootStoreIntovalue(thisReceiver, newValue);
		}
		longAtput(((((char *) thisReceiver)) + 4) + ((index - 1) << 2), newValue);
		/* begin push: */
		longAtput(sp = stackPointer + 4, newValue);
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 3 * 4;
	}
}

int primitiveObjectPointsTo(void) {
    int i;
    int thang;
    int lastField;
    int rcvr;
    int top;
    int top1;
    int sp;
    int sp1;
    int sp2;
    int sp3;
    int sp4;
    int sp5;
    int methodHeader;
    int sz;
    int fmt;
    int header;
    int header1;
    int type;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	thang = top;
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	rcvr = top1;
	if ((rcvr & 1)) {
		/* begin pushBool: */
				/* begin push: */
		longAtput(sp1 = stackPointer + 4, falseObj);
		stackPointer = sp1;
;
		return 0;
	}
	/* begin lastPointerOf: */
	header = longAt(rcvr);
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			lastField = (6 + (fetchStackPointerOf(rcvr))) * 4;
			goto l1;
		}
		/* begin sizeBitsOfSafe: */
		header1 = longAt(rcvr);
		/* begin rightType: */
		if ((header1 & 252) == 0) {
			type = 0;
			goto l2;
		} else {
			if ((header1 & 126976) == 0) {
				type = 1;
				goto l2;
			} else {
				type = 3;
				goto l2;
			}
		}
	l2:	/* end rightType: */;
		if (type == 0) {
			sz = (longAt(rcvr - 8)) & 4294967292U;
			goto l3;
		} else {
			sz = header1 & 252;
			goto l3;
		}
	l3:	/* end sizeBitsOfSafe: */;
		lastField = sz - 4;
		goto l1;
	}
	if (fmt < 12) {
		lastField = 0;
		goto l1;
	}
	methodHeader = longAt(rcvr + 4);
	lastField = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
l1:	/* end lastPointerOf: */;
	for (i = 4; i <= lastField; i += 4) {
		if ((longAt(rcvr + i)) == thang) {
			/* begin pushBool: */
						/* begin push: */
			longAtput(sp2 = stackPointer + 4, trueObj);
			stackPointer = sp2;
;
			return 0;
		}
	}
	/* begin pushBool: */
		/* begin push: */
	longAtput(sp5 = stackPointer + 4, falseObj);
	stackPointer = sp5;
;
}

void primitivePerform(void) {
    int performSelector;
    int selectorIndex;
    int newReceiver;
    int lookupClass;
    int toIndex;
    int fromIndex;
    int lastFrom;
    int ccIndex;
    int methodHeader;
    int i;
    int nilOop;
    int tempCount;
    int newContext;
    int initialIP;
    int cntxt;
    int tmp;

	performSelector = messageSelector;
	messageSelector = longAt(stackPointer - ((argumentCount - 1) * 4));
	newReceiver = longAt(stackPointer - (argumentCount * 4));
	argumentCount -= 1;
	/* begin fetchClassOf: */
	if ((newReceiver & 1)) {
		lookupClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l2;
	}
	ccIndex = (((unsigned) (longAt(newReceiver))) >> 12) & 31;
	if (ccIndex == 0) {
		lookupClass = (longAt(newReceiver - 4)) & 4294967292U;
		goto l2;
	} else {
		lookupClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l2;
	}
l2:	/* end fetchClassOf: */;
	findNewMethodInClass(lookupClass);
	successFlag = (((((unsigned) (longAt(((((char *) newMethod)) + 4) + (0 << 2)))) >> 25) & 15) == argumentCount) && successFlag;
	if (successFlag) {
		selectorIndex = (((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - argumentCount;
		/* begin transfer:fromIndex:ofObject:toIndex:ofObject: */
		fromIndex = activeContext + ((selectorIndex + 1) * 4);
		toIndex = activeContext + (selectorIndex * 4);
		lastFrom = fromIndex + (argumentCount * 4);
		while (fromIndex < lastFrom) {
			fromIndex += 4;
			toIndex += 4;
			longAtput(toIndex, longAt(fromIndex));
		}
		/* begin pop: */
		stackPointer -= 1 * 4;
		/* begin executeNewMethod */
		if (primitiveIndex > 0) {
			primitiveResponse();
			if (successFlag) {
				goto l1;
			}
		}
		/* begin activateNewMethod */
		methodHeader = longAt(((((char *) newMethod)) + 4) + (0 << 2));
		/* begin allocateOrRecycleContext */
		if (freeContexts != 1) {
			cntxt = freeContexts;
			freeContexts = longAt(((((char *) cntxt)) + 4) + (0 << 2));
			newContext = cntxt;
			goto l3;
		}
		cntxt = instantiateContextsizeInBytes(longAt(((((char *) specialObjectsOop)) + 4) + (10 << 2)), 156);
		longAtput(((((char *) cntxt)) + 4) + (4 << 2), nilObj);
		newContext = cntxt;
	l3:	/* end allocateOrRecycleContext */;
		initialIP = ((1 + ((((unsigned) methodHeader) >> 10) & 255)) * 4) + 1;
		tempCount = (((unsigned) methodHeader) >> 19) & 63;
		longAtput(((((char *) newContext)) + 4) + (0 << 2), activeContext);
		longAtput(((((char *) newContext)) + 4) + (1 << 2), ((initialIP << 1) | 1));
		longAtput(((((char *) newContext)) + 4) + (2 << 2), ((tempCount << 1) | 1));
		longAtput(((((char *) newContext)) + 4) + (3 << 2), newMethod);
		for (i = 0; i <= argumentCount; i += 1) {
			longAtput(((((char *) newContext)) + 4) + ((5 + i) << 2), longAt(stackPointer - ((argumentCount - i) * 4)));
		}
		nilOop = nilObj;
		for (i = (argumentCount + 1); i <= tempCount; i += 1) {
			longAtput(((((char *) newContext)) + 4) + ((5 + i) << 2), nilOop);
		}
		/* begin pop: */
		stackPointer -= (argumentCount + 1) * 4;
		reclaimableContextCount += 1;
		/* begin newActiveContext: */
		/* begin storeContextRegisters: */
		longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
		longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
		if (newContext < youngStart) {
			beRootIfOld(newContext);
		}
		activeContext = newContext;
		/* begin fetchContextRegisters: */
		tmp = longAt(((((char *) newContext)) + 4) + (3 << 2));
		if ((tmp & 1)) {
			tmp = longAt(((((char *) newContext)) + 4) + (5 << 2));
			if (tmp < youngStart) {
				beRootIfOld(tmp);
			}
		} else {
			tmp = newContext;
		}
		theHomeContext = tmp;
		receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
		method = longAt(((((char *) tmp)) + 4) + (3 << 2));
		tmp = ((longAt(((((char *) newContext)) + 4) + (1 << 2))) >> 1);
		instructionPointer = ((method + tmp) + 4) - 2;
		tmp = ((longAt(((((char *) newContext)) + 4) + (2 << 2))) >> 1);
		stackPointer = (newContext + 4) + (((6 + tmp) - 1) * 4);
		/* begin quickCheckForInterrupts */
		if ((interruptCheckCounter -= 1) <= 0) {
			checkForInterrupts();
		}
	l1:	/* end executeNewMethod */;
		successFlag = 1;
	} else {
		argumentCount += 1;
		messageSelector = performSelector;
	}
}

void primitivePerformWithArgs(void) {
    int argumentArray;
    int performSelector;
    int arraySize;
    int thisReceiver;
    int index;
    int cntxSize;
    int lookupClass;
    int sp;
    int sp1;
    int sp2;
    int top;
    int ccIndex;
    int top1;
    int sz;
    int header;
    int sz1;
    int header1;
    int methodHeader;
    int i;
    int nilOop;
    int tempCount;
    int newContext;
    int initialIP;
    int cntxt;
    int tmp;

	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	argumentArray = top1;
	/* begin fetchWordLengthOf: */
	/* begin sizeBitsOf: */
	header = longAt(argumentArray);
	if ((header & 3) == 0) {
		sz = (longAt(argumentArray - 8)) & 4294967292U;
		goto l3;
	} else {
		sz = header & 252;
		goto l3;
	}
l3:	/* end sizeBitsOf: */;
	arraySize = ((unsigned) (sz - 4)) >> 2;
	/* begin fetchWordLengthOf: */
	/* begin sizeBitsOf: */
	header1 = longAt(activeContext);
	if ((header1 & 3) == 0) {
		sz1 = (longAt(activeContext - 8)) & 4294967292U;
		goto l4;
	} else {
		sz1 = header1 & 252;
		goto l4;
	}
l4:	/* end sizeBitsOf: */;
	cntxSize = ((unsigned) (sz1 - 4)) >> 2;
	successFlag = (((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) + arraySize) < cntxSize) && successFlag;
	if (successFlag) {
		performSelector = messageSelector;
		/* begin popStack */
		top = longAt(stackPointer);
		stackPointer -= 4;
		messageSelector = top;
		thisReceiver = longAt(stackPointer);
		argumentCount = arraySize;
		index = 1;
		while (index <= argumentCount) {
			/* begin push: */
			longAtput(sp = stackPointer + 4, longAt(((((char *) argumentArray)) + 4) + ((index - 1) << 2)));
			stackPointer = sp;
			index += 1;
		}
		/* begin fetchClassOf: */
		if ((thisReceiver & 1)) {
			lookupClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l2;
		}
		ccIndex = (((unsigned) (longAt(thisReceiver))) >> 12) & 31;
		if (ccIndex == 0) {
			lookupClass = (longAt(thisReceiver - 4)) & 4294967292U;
			goto l2;
		} else {
			lookupClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l2;
		}
	l2:	/* end fetchClassOf: */;
		findNewMethodInClass(lookupClass);
		successFlag = (((((unsigned) (longAt(((((char *) newMethod)) + 4) + (0 << 2)))) >> 25) & 15) == argumentCount) && successFlag;
		if (successFlag) {
			/* begin executeNewMethod */
			if (primitiveIndex > 0) {
				primitiveResponse();
				if (successFlag) {
					goto l1;
				}
			}
			/* begin activateNewMethod */
			methodHeader = longAt(((((char *) newMethod)) + 4) + (0 << 2));
			/* begin allocateOrRecycleContext */
			if (freeContexts != 1) {
				cntxt = freeContexts;
				freeContexts = longAt(((((char *) cntxt)) + 4) + (0 << 2));
				newContext = cntxt;
				goto l5;
			}
			cntxt = instantiateContextsizeInBytes(longAt(((((char *) specialObjectsOop)) + 4) + (10 << 2)), 156);
			longAtput(((((char *) cntxt)) + 4) + (4 << 2), nilObj);
			newContext = cntxt;
		l5:	/* end allocateOrRecycleContext */;
			initialIP = ((1 + ((((unsigned) methodHeader) >> 10) & 255)) * 4) + 1;
			tempCount = (((unsigned) methodHeader) >> 19) & 63;
			longAtput(((((char *) newContext)) + 4) + (0 << 2), activeContext);
			longAtput(((((char *) newContext)) + 4) + (1 << 2), ((initialIP << 1) | 1));
			longAtput(((((char *) newContext)) + 4) + (2 << 2), ((tempCount << 1) | 1));
			longAtput(((((char *) newContext)) + 4) + (3 << 2), newMethod);
			for (i = 0; i <= argumentCount; i += 1) {
				longAtput(((((char *) newContext)) + 4) + ((5 + i) << 2), longAt(stackPointer - ((argumentCount - i) * 4)));
			}
			nilOop = nilObj;
			for (i = (argumentCount + 1); i <= tempCount; i += 1) {
				longAtput(((((char *) newContext)) + 4) + ((5 + i) << 2), nilOop);
			}
			/* begin pop: */
			stackPointer -= (argumentCount + 1) * 4;
			reclaimableContextCount += 1;
			/* begin newActiveContext: */
			/* begin storeContextRegisters: */
			longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
			longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
			if (newContext < youngStart) {
				beRootIfOld(newContext);
			}
			activeContext = newContext;
			/* begin fetchContextRegisters: */
			tmp = longAt(((((char *) newContext)) + 4) + (3 << 2));
			if ((tmp & 1)) {
				tmp = longAt(((((char *) newContext)) + 4) + (5 << 2));
				if (tmp < youngStart) {
					beRootIfOld(tmp);
				}
			} else {
				tmp = newContext;
			}
			theHomeContext = tmp;
			receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
			method = longAt(((((char *) tmp)) + 4) + (3 << 2));
			tmp = ((longAt(((((char *) newContext)) + 4) + (1 << 2))) >> 1);
			instructionPointer = ((method + tmp) + 4) - 2;
			tmp = ((longAt(((((char *) newContext)) + 4) + (2 << 2))) >> 1);
			stackPointer = (newContext + 4) + (((6 + tmp) - 1) * 4);
			/* begin quickCheckForInterrupts */
			if ((interruptCheckCounter -= 1) <= 0) {
				checkForInterrupts();
			}
		l1:	/* end executeNewMethod */;
			successFlag = 1;
		} else {
			/* begin pop: */
			stackPointer -= argumentCount * 4;
			/* begin push: */
			longAtput(sp1 = stackPointer + 4, messageSelector);
			stackPointer = sp1;
			/* begin push: */
			longAtput(sp2 = stackPointer + 4, argumentArray);
			stackPointer = sp2;
			argumentCount = 2;
			messageSelector = performSelector;
		}
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitivePointX(void) {
    int rcvr;
    int sp;
    int top;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	rcvr = top;
	if (successFlag) {
		/* begin push: */
		longAtput(sp = stackPointer + 4, longAt(((((char *) rcvr)) + 4) + (0 << 2)));
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitivePointY(void) {
    int rcvr;
    int sp;
    int top;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	rcvr = top;
	if (successFlag) {
		/* begin push: */
		longAtput(sp = stackPointer + 4, longAt(((((char *) rcvr)) + 4) + (1 << 2)));
		stackPointer = sp;
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitivePushFalse(void) {
    int top;
    int sp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, falseObj);
	stackPointer = sp;
}

void primitivePushMinusOne(void) {
    int top;
    int sp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, 4294967295U);
	stackPointer = sp;
}

void primitivePushNil(void) {
    int top;
    int sp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, nilObj);
	stackPointer = sp;
}

void primitivePushOne(void) {
    int top;
    int sp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, 3);
	stackPointer = sp;
}

void primitivePushSelf(void) {
}

void primitivePushTrue(void) {
    int top;
    int sp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, trueObj);
	stackPointer = sp;
}

void primitivePushTwo(void) {
    int top;
    int sp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, 5);
	stackPointer = sp;
}

void primitivePushZero(void) {
    int top;
    int sp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, 1);
	stackPointer = sp;
}

int primitivePutChar(void) {
    int ch;
    int integerPointer;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (0 * 4));
	if ((integerPointer & 1)) {
		ch = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		ch = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	if (!(successFlag)) {
		return successFlag = 0;
	}
	ioPutChar(ch);
	/* begin pop: */
	stackPointer -= 2 * 4;
}

void primitiveQuit(void) {
	ioExit();
}

void primitiveQuo(void) {
    int integerResult;
    int integerRcvr;
    int integerArg;
    int integerPointer;
    int integerPointer1;
    int sp;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (1 * 4));
	if ((integerPointer & 1)) {
		integerRcvr = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		integerRcvr = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (0 * 4));
	if ((integerPointer1 & 1)) {
		integerArg = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		integerArg = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	successFlag = (integerArg != 0) && successFlag;
	if (successFlag) {
		if (integerRcvr > 0) {
			if (integerArg > 0) {
				integerResult = integerRcvr / integerArg;
			} else {
				integerResult = 0 - (integerRcvr / (0 - integerArg));
			}
		} else {
			if (integerArg > 0) {
				integerResult = 0 - ((0 - integerRcvr) / integerArg);
			} else {
				integerResult = (0 - integerRcvr) / (0 - integerArg);
			}
		}
	}
	/* begin pop2AndPushIntegerIfOK: */
	if (successFlag) {
		if ((integerResult ^ (integerResult << 1)) >= 0) {
			/* begin pop:thenPush: */
			longAtput(sp = stackPointer - ((2 - 1) * 4), ((integerResult << 1) | 1));
			stackPointer = sp;
		} else {
			successFlag = 0;
		}
	}
}

int primitiveResponse(void) {
    int startTime;
    int timerPending;

	timerPending = nextWakeupTick != 0;
	if (timerPending) {
		startTime = ioLowResMSecs();
	}
	successFlag = 1;
	switch (primitiveIndex) {
	case 0:
		successFlag = 0;
		break;
	case 1:
		primitiveAdd();
		break;
	case 2:
		primitiveSubtract();
		break;
	case 3:
		primitiveLessThan();
		break;
	case 4:
		primitiveGreaterThan();
		break;
	case 5:
		primitiveLessOrEqual();
		break;
	case 6:
		primitiveGreaterOrEqual();
		break;
	case 7:
		primitiveEqual();
		break;
	case 8:
		primitiveNotEqual();
		break;
	case 9:
		primitiveMultiply();
		break;
	case 10:
		primitiveDivide();
		break;
	case 11:
		primitiveMod();
		break;
	case 12:
		primitiveDiv();
		break;
	case 13:
		primitiveQuo();
		break;
	case 14:
		primitiveBitAnd();
		break;
	case 15:
		primitiveBitOr();
		break;
	case 16:
		primitiveBitXor();
		break;
	case 17:
		primitiveBitShift();
		break;
	case 18:
		primitiveMakePoint();
		break;
	case 19:
	case 20:
	case 21:
	case 22:
	case 23:
	case 24:
	case 25:
	case 26:
	case 27:
	case 28:
	case 29:
	case 30:
	case 31:
	case 32:
	case 33:
	case 34:
	case 35:
	case 36:
	case 37:
	case 38:
	case 39:
		successFlag = 0;
		break;
	case 40:
		primitiveAsFloat();
		break;
	case 41:
		primitiveFloatAdd();
		break;
	case 42:
		primitiveFloatSubtract();
		break;
	case 43:
		primitiveFloatLessThan();
		break;
	case 44:
		primitiveFloatGreaterThan();
		break;
	case 45:
		primitiveFloatLessOrEqual();
		break;
	case 46:
		primitiveFloatGreaterOrEqual();
		break;
	case 47:
		primitiveFloatEqual();
		break;
	case 48:
		primitiveFloatNotEqual();
		break;
	case 49:
		primitiveFloatMultiply();
		break;
	case 50:
		primitiveFloatDivide();
		break;
	case 51:
		primitiveTruncated();
		break;
	case 52:
		primitiveFractionalPart();
		break;
	case 53:
		primitiveExponent();
		break;
	case 54:
		primitiveTimesTwoPower();
		break;
	case 55:
		primitiveSquareRoot();
		break;
	case 56:
		primitiveSine();
		break;
	case 57:
		primitiveArctan();
		break;
	case 58:
		primitiveLogN();
		break;
	case 59:
		primitiveExp();
		break;
	case 60:
		primitiveAt();
		break;
	case 61:
		primitiveAtPut();
		break;
	case 62:
		primitiveSize();
		break;
	case 63:
		primitiveStringAt();
		break;
	case 64:
		primitiveStringAtPut();
		break;
	case 65:
		primitiveNext();
		break;
	case 66:
		primitiveNextPut();
		break;
	case 67:
		primitiveAtEnd();
		break;
	case 68:
		primitiveObjectAt();
		break;
	case 69:
		primitiveObjectAtPut();
		break;
	case 70:
		primitiveNew();
		break;
	case 71:
		primitiveNewWithArg();
		break;
	case 72:
		primitiveArrayBecomeOneWay();
		break;
	case 73:
		primitiveInstVarAt();
		break;
	case 74:
		primitiveInstVarAtPut();
		break;
	case 75:
		primitiveAsOop();
		break;
	case 76:
		primitiveStoreStackp();
		break;
	case 77:
		primitiveSomeInstance();
		break;
	case 78:
		primitiveNextInstance();
		break;
	case 79:
		primitiveNewMethod();
		break;
	case 80:
		primitiveBlockCopy();
		break;
	case 81:
		primitiveValue();
		break;
	case 82:
		primitiveValueWithArgs();
		break;
	case 83:
		primitivePerform();
		break;
	case 84:
		primitivePerformWithArgs();
		break;
	case 85:
		primitiveSignal();
		break;
	case 86:
		primitiveWait();
		break;
	case 87:
		primitiveResume();
		break;
	case 88:
		primitiveSuspend();
		break;
	case 89:
		primitiveFlushCache();
		break;
	case 90:
		primitiveMousePoint();
		break;
	case 91:
	case 92:
	case 93:
	case 94:
	case 95:
		successFlag = 0;
		break;
	case 96:
		primitiveCopyBits();
		break;
	case 97:
	case 98:
	case 99:
	case 100:
		successFlag = 0;
		break;
	case 101:
		primitiveBeCursor();
		break;
	case 102:
		primitiveBeDisplay();
		break;
	case 103:
		successFlag = 0;
		break;
	case 104:
		primitiveDrawLoop();
		break;
	case 105:
		primitiveStringReplace();
		break;
	case 106:
		primitiveScreenSize();
		break;
	case 107:
		primitiveMouseButtons();
		break;
	case 108:
		primitiveKbdNext();
		break;
	case 109:
		primitiveKbdPeek();
		break;
	case 110:
		primitiveEquivalent();
		break;
	case 111:
		primitiveClass();
		break;
	case 112:
		primitiveBytesLeft();
		break;
	case 113:
		primitiveQuit();
		break;
	case 114:
		primitiveExitToDebugger();
		break;
	case 115:
		successFlag = 0;
		break;
	case 116:
		primitiveFlushCacheByMethod();
		break;
	case 117:
		successFlag = 0;
		break;
	case 118:
		primitiveDoPrimitiveWithArgs();
		break;
	case 119:
		primitiveFlushCacheSelective();
		break;
	case 120:
		successFlag = 0;
		break;
	case 121:
		primitiveImageName();
		break;
	case 122:
	case 123:
		successFlag = 0;
		break;
	case 124:
		primitiveLowSpaceSemaphore();
		break;
	case 125:
		primitiveSignalAtBytesLeft();
		break;
	case 126:
		primitiveDeferDisplayUpdates();
		break;
	case 127:
		primitiveShowDisplayRect();
		break;
	case 128:
		primitiveArrayBecome();
		break;
	case 129:
		primitiveSpecialObjectsOop();
		break;
	case 130:
		primitiveFullGC();
		break;
	case 131:
		primitiveIncrementalGC();
		break;
	case 132:
		primitiveObjectPointsTo();
		break;
	case 133:
		primitiveSetInterruptKey();
		break;
	case 134:
		primitiveInterruptSemaphore();
		break;
	case 135:
		primitiveMillisecondClock();
		break;
	case 136:
		primitiveSignalAtMilliseconds();
		break;
	case 137:
		primitiveSecondsClock();
		break;
	case 138:
		primitiveSomeObject();
		break;
	case 139:
		primitiveNextObject();
		break;
	case 140:
		primitiveBeep();
		break;
	case 141:
	case 142:
		successFlag = 0;
		break;
	case 143:
		primitiveShortAt();
		break;
	case 144:
		primitiveShortAtPut();
		break;
	case 145:
		primitiveConstantFill();
		break;
	case 146:
		successFlag = 0;
		break;
	case 147:
		primitiveWarpBits();
		break;
	case 148:
		primitiveClone();
		break;
	case 149:
		primitiveGetAttribute();
		break;
	case 150:
		primitiveFileAtEnd();
		break;
	case 151:
		primitiveFileClose();
		break;
	case 152:
		primitiveFileGetPosition();
		break;
	case 153:
		primitiveFileOpen();
		break;
	case 154:
		primitiveFileRead();
		break;
	case 155:
		primitiveFileSetPosition();
		break;
	case 156:
		primitiveFileDelete();
		break;
	case 157:
		primitiveFileSize();
		break;
	case 158:
		primitiveFileWrite();
		break;
	case 159:
		primitiveFileRename();
		break;
	case 160:
		successFlag = 0;
		break;
	case 161:
		primitiveDirectoryDelimitor();
		break;
	case 162:
	case 163:
	case 164:
	case 165:
	case 166:
	case 167:
	case 168:
	case 169:
	case 170:
	case 171:
		successFlag = 0;
		break;
	case 172:
		primitiveSoundStop();
		break;
	case 173:
	case 174:
	case 175:
	case 176:
	case 177:
	case 178:
	case 179:
	case 180:
	case 181:
	case 182:
	case 183:
	case 184:
	case 185:
	case 186:
	case 187:
	case 188:
	case 189:
	case 190:
	case 191:
	case 192:
	case 193:
	case 194:
	case 195:
	case 196:
	case 197:
	case 198:
	case 199:
	case 200:
	case 201:
	case 202:
	case 203:
	case 204:
	case 205:
	case 206:
	case 207:
	case 208:
	case 209:
	case 210:
	case 211:
	case 212:
	case 213:
	case 214:
	case 215:
	case 216:
	case 217:
	case 218:
	case 219:
	case 220:
	case 221:
	case 222:
	case 223:
	case 224:
	case 225:
	case 226:
	case 227:
	case 228:
	case 229:
	case 230:
	case 231:
	case 232:
		successFlag = 0;
		break;
	case 233:
		primitiveSetFullScreen();
		break;
	case 234:
		primBitmapdecompressfromByteArrayat();
		break;
	case 235:
		primStringcomparewithcollated();
		break;
	case 236:
		successFlag = 0;
		break;
	case 237:
		primBitmapcompresstoByteArray();
		break;
	case 238:
	case 239:
	case 240:
	case 241:
	case 242:
		successFlag = 0;
		break;
	case 243:
		primStringtranslatefromtotable();
		break;
	case 244:
		primStringfindFirstInStringinSetstartingAt();
		break;
	case 245:
		primStringindexOfAsciiinStringstartingAt();
		break;
	case 246:
		primStringfindSubstringinstartingAtmatchTable();
		break;
	case 247:
	case 248:
		successFlag = 0;
		break;
	case 249:
		primitivePutChar();
		break;
	case 250:
	case 251:
	case 252:
	case 253:
		successFlag = 0;
		break;
	case 254:
		primitiveVMParameter();
		break;
	case 255:
		successFlag = 0;
		break;
	case 256:
		primitivePushSelf();
		break;
	case 257:
		primitivePushTrue();
		break;
	case 258:
		primitivePushFalse();
		break;
	case 259:
		primitivePushNil();
		break;
	case 260:
		primitivePushMinusOne();
		break;
	case 261:
		primitivePushZero();
		break;
	case 262:
		primitivePushOne();
		break;
	case 263:
		primitivePushTwo();
		break;
	case 264:
	case 265:
	case 266:
	case 267:
	case 268:
	case 269:
	case 270:
	case 271:
	case 272:
	case 273:
	case 274:
	case 275:
	case 276:
	case 277:
	case 278:
	case 279:
	case 280:
	case 281:
	case 282:
	case 283:
	case 284:
	case 285:
	case 286:
	case 287:
	case 288:
	case 289:
	case 290:
	case 291:
	case 292:
	case 293:
	case 294:
	case 295:
	case 296:
	case 297:
	case 298:
	case 299:
	case 300:
	case 301:
	case 302:
	case 303:
	case 304:
	case 305:
	case 306:
	case 307:
	case 308:
	case 309:
	case 310:
	case 311:
	case 312:
	case 313:
	case 314:
	case 315:
	case 316:
	case 317:
	case 318:
	case 319:
	case 320:
	case 321:
	case 322:
	case 323:
	case 324:
	case 325:
	case 326:
	case 327:
	case 328:
	case 329:
	case 330:
	case 331:
	case 332:
	case 333:
	case 334:
	case 335:
	case 336:
	case 337:
	case 338:
	case 339:
	case 340:
	case 341:
	case 342:
	case 343:
	case 344:
	case 345:
	case 346:
	case 347:
	case 348:
	case 349:
	case 350:
	case 351:
	case 352:
	case 353:
	case 354:
	case 355:
	case 356:
	case 357:
	case 358:
	case 359:
	case 360:
	case 361:
	case 362:
	case 363:
	case 364:
	case 365:
	case 366:
	case 367:
	case 368:
	case 369:
	case 370:
	case 371:
	case 372:
	case 373:
	case 374:
	case 375:
	case 376:
	case 377:
	case 378:
	case 379:
	case 380:
	case 381:
	case 382:
	case 383:
	case 384:
	case 385:
	case 386:
	case 387:
	case 388:
	case 389:
	case 390:
	case 391:
	case 392:
	case 393:
	case 394:
	case 395:
	case 396:
	case 397:
	case 398:
	case 399:
	case 400:
	case 401:
	case 402:
	case 403:
	case 404:
	case 405:
	case 406:
	case 407:
	case 408:
	case 409:
	case 410:
	case 411:
	case 412:
	case 413:
	case 414:
	case 415:
	case 416:
	case 417:
	case 418:
	case 419:
	case 420:
	case 421:
	case 422:
	case 423:
	case 424:
	case 425:
	case 426:
	case 427:
	case 428:
	case 429:
	case 430:
	case 431:
	case 432:
	case 433:
	case 434:
	case 435:
	case 436:
	case 437:
	case 438:
	case 439:
	case 440:
	case 441:
	case 442:
	case 443:
	case 444:
	case 445:
	case 446:
	case 447:
	case 448:
	case 449:
	case 450:
	case 451:
	case 452:
	case 453:
	case 454:
	case 455:
	case 456:
	case 457:
	case 458:
	case 459:
	case 460:
	case 461:
	case 462:
	case 463:
	case 464:
	case 465:
	case 466:
	case 467:
	case 468:
	case 469:
	case 470:
	case 471:
	case 472:
	case 473:
	case 474:
	case 475:
	case 476:
	case 477:
	case 478:
	case 479:
	case 480:
	case 481:
	case 482:
	case 483:
	case 484:
	case 485:
	case 486:
	case 487:
	case 488:
	case 489:
	case 490:
	case 491:
	case 492:
	case 493:
	case 494:
	case 495:
	case 496:
	case 497:
	case 498:
	case 499:
	case 500:
	case 501:
	case 502:
	case 503:
	case 504:
	case 505:
	case 506:
	case 507:
	case 508:
	case 509:
	case 510:
	case 511:
	case 512:
	case 513:
	case 514:
	case 515:
	case 516:
	case 517:
	case 518:
	case 519:
		primitiveLoadInstVar();
		break;
	case 520:
		successFlag = 0;
		break;
	}
	if (timerPending) {
		if ((ioLowResMSecs()) != startTime) {
			if (((ioMSecs()) & 536870911) >= nextWakeupTick) {
				if (successFlag) {
					checkForInterrupts();
				} else {
					interruptCheckCounter = 0;
				}
			}
		}
	}
	return successFlag;
}

void primitiveResume(void) {
    int proc;

	proc = longAt(stackPointer);
	if (successFlag) {
		resume(proc);
	}
}

void primitiveScreenSize(void) {
    int pointWord;
    int object;
    int sp;
    int pointResult;

	/* begin pop: */
	stackPointer -= 1 * 4;
	pointWord = ioScreenSize();
	/* begin push: */
	/* begin makePointwithxValue:yValue: */
	pointResult = instantiateSmallClasssizeInBytesfill(longAt(((((char *) specialObjectsOop)) + 4) + (12 << 2)), 12, nilObj);
	/* begin storePointer:ofObject:withValue: */
	if (pointResult < youngStart) {
		possibleRootStoreIntovalue(pointResult, ((((((unsigned) pointWord) >> 16) & 65535) << 1) | 1));
	}
	longAtput(((((char *) pointResult)) + 4) + (0 << 2), ((((((unsigned) pointWord) >> 16) & 65535) << 1) | 1));
	/* begin storePointer:ofObject:withValue: */
	if (pointResult < youngStart) {
		possibleRootStoreIntovalue(pointResult, (((pointWord & 65535) << 1) | 1));
	}
	longAtput(((((char *) pointResult)) + 4) + (1 << 2), (((pointWord & 65535) << 1) | 1));
	object = pointResult;
	longAtput(sp = stackPointer + 4, object);
	stackPointer = sp;
}

void primitiveSecondsClock(void) {
    int object;
    int sp;

	/* begin pop: */
	stackPointer -= 1 * 4;
	/* begin push: */
	object = positive32BitIntegerFor(ioSeconds());
	longAtput(sp = stackPointer + 4, object);
	stackPointer = sp;
}

void primitiveSetFullScreen(void) {
    int argOop;

	argOop = longAt(stackPointer);
	if (argOop == trueObj) {
		ioSetFullScreen(1);
	} else {
		if (argOop == falseObj) {
			ioSetFullScreen(0);
		} else {
			successFlag = 0;
		}
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 1 * 4;
	}
}

void primitiveSetInterruptKey(void) {
    int keycode;
    int integerPointer;
    int top;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		keycode = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		keycode = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	if (successFlag) {
		interruptKeycode = keycode;
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

int primitiveShortAt(void) {
    int addr;
    int sz;
    int value;
    int index;
    int rcvr;
    int successValue;
    int successValue1;
    int sp;
    int integerPointer;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (0 * 4));
	if ((integerPointer & 1)) {
		index = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		index = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	rcvr = longAt(stackPointer - (1 * 4));
	/* begin success: */
	successValue = (!((rcvr & 1))) && (isWordsOrBytes(rcvr));
	successFlag = successValue && successFlag;
	if (!(successFlag)) {
		return null;
	}
	sz = ((int) ((sizeBitsOf(rcvr)) - 4) >> 1);
	/* begin success: */
	successValue1 = (index >= 1) && (index <= sz);
	successFlag = successValue1 && successFlag;
	if (successFlag) {
		addr = (rcvr + 4) + (2 * (index - 1));
		value = *((short int *) addr);
		/* begin pop: */
		stackPointer -= 2 * 4;
		/* begin pushInteger: */
		/* begin push: */
		longAtput(sp = stackPointer + 4, ((value << 1) | 1));
		stackPointer = sp;
	}
}

int primitiveShortAtPut(void) {
    int addr;
    int sz;
    int value;
    int index;
    int rcvr;
    int integerPointer;
    int integerPointer1;
    int successValue;
    int successValue1;
    int successValue2;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (0 * 4));
	if ((integerPointer & 1)) {
		value = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		value = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (1 * 4));
	if ((integerPointer1 & 1)) {
		index = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		index = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	rcvr = longAt(stackPointer - (2 * 4));
	/* begin success: */
	successValue = (!((rcvr & 1))) && (isWordsOrBytes(rcvr));
	successFlag = successValue && successFlag;
	if (!(successFlag)) {
		return null;
	}
	sz = ((int) ((sizeBitsOf(rcvr)) - 4) >> 1);
	/* begin success: */
	successValue1 = (index >= 1) && (index <= sz);
	successFlag = successValue1 && successFlag;
	/* begin success: */
	successValue2 = (value >= -32768) && (value <= 32767);
	successFlag = successValue2 && successFlag;
	if (successFlag) {
		addr = (rcvr + 4) + (2 * (index - 1));
		*((short int *) addr) = value;
		/* begin pop: */
		stackPointer -= 2 * 4;
	}
}

void primitiveShowDisplayRect(void) {
    int displayObj;
    int dispBits;
    int dispBitsPtr;
    int top;
    int bottom;
    int h;
    int w;
    int d;
    int right;
    int left;
    int integerPointer;
    int integerPointer1;
    int integerPointer2;
    int integerPointer3;
    int successValue;
    int successValue1;

	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (0 * 4));
	if ((integerPointer & 1)) {
		bottom = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		bottom = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (1 * 4));
	if ((integerPointer1 & 1)) {
		top = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		top = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer2 = longAt(stackPointer - (2 * 4));
	if ((integerPointer2 & 1)) {
		right = (integerPointer2 >> 1);
		goto l3;
	} else {
		successFlag = 0;
		right = 0;
		goto l3;
	}
l3:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer3 = longAt(stackPointer - (3 * 4));
	if ((integerPointer3 & 1)) {
		left = (integerPointer3 >> 1);
		goto l4;
	} else {
		successFlag = 0;
		left = 0;
		goto l4;
	}
l4:	/* end stackIntegerValue: */;
	displayObj = longAt(((((char *) specialObjectsOop)) + 4) + (14 << 2));
	/* begin success: */
	successValue = (((((unsigned) (longAt(displayObj))) >> 8) & 15) <= 4) && ((lengthOf(displayObj)) >= 4);
	successFlag = successValue && successFlag;
	if (successFlag) {
		dispBits = longAt(((((char *) displayObj)) + 4) + (0 << 2));
		w = fetchIntegerofObject(1, displayObj);
		h = fetchIntegerofObject(2, displayObj);
		d = fetchIntegerofObject(3, displayObj);
	}
	if (left < 0) {
		left = 0;
	}
	if (right > w) {
		right = w;
	}
	if (top < 0) {
		top = 0;
	}
	if (bottom > h) {
		bottom = h;
	}
	/* begin success: */
	successValue1 = (left <= right) && (top <= bottom);
	successFlag = successValue1 && successFlag;
	if (successFlag) {
		dispBitsPtr = dispBits + 4;
		ioShowDisplay(dispBitsPtr, w, h, d, left, right, top, bottom);
	}
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 4 * 4;
	}
}

void primitiveSignal(void) {
    int sema;

	sema = longAt(stackPointer);
	if (successFlag) {
		synchronousSignal(sema);
	}
}

void primitiveSignalAtBytesLeft(void) {
    int bytes;
    int integerPointer;
    int top;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		bytes = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		bytes = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	if (successFlag) {
		lowSpaceThreshold = bytes;
	} else {
		lowSpaceThreshold = 0;
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveSignalAtMilliseconds(void) {
    int sema;
    int tick;
    int oop;
    int oop1;
    int valuePointer;
    int integerPointer;
    int top;
    int top1;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		tick = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		tick = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	sema = top1;
	if (successFlag) {
		if ((fetchClassOf(sema)) == (longAt(((((char *) specialObjectsOop)) + 4) + (18 << 2)))) {
			/* begin storePointer:ofObject:withValue: */
			oop = specialObjectsOop;
			if (oop < youngStart) {
				possibleRootStoreIntovalue(oop, sema);
			}
			longAtput(((((char *) oop)) + 4) + (29 << 2), sema);
			nextWakeupTick = tick;
		} else {
			/* begin storePointer:ofObject:withValue: */
			oop1 = specialObjectsOop;
			valuePointer = nilObj;
			if (oop1 < youngStart) {
				possibleRootStoreIntovalue(oop1, valuePointer);
			}
			longAtput(((((char *) oop1)) + 4) + (29 << 2), valuePointer);
			nextWakeupTick = 0;
		}
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveSine(void) {
    double rcvr;

	rcvr = popFloat();
	if (successFlag) {
		pushFloat(sin(rcvr));
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

void primitiveSize(void) {
    int sz;
    int rcvr;
    int hdr;
    int totalLength;
    int fmt;
    int fixedFields;
    int oop;
    int sp;
    int sp1;
    int sz1;
    int classFormat;
    int cls;
    int ccIndex;

	rcvr = longAt(stackPointer);
	if ((rcvr & 1)) {
		successFlag = 0;
	} else {
		/* begin stSizeOf: */
		hdr = longAt(rcvr);
		fmt = (((unsigned) hdr) >> 8) & 15;
		/* begin lengthOf:baseHeader:format: */
		if ((hdr & 3) == 0) {
			sz1 = (longAt(rcvr - 8)) & 4294967292U;
		} else {
			sz1 = hdr & 252;
		}
		if (fmt < 8) {
			totalLength = ((unsigned) (sz1 - 4)) >> 2;
			goto l3;
		} else {
			totalLength = (sz1 - 4) - (fmt & 3);
			goto l3;
		}
	l3:	/* end lengthOf:baseHeader:format: */;
		/* begin fixedFieldsOf:format:length: */
		if ((fmt > 4) || (fmt == 2)) {
			fixedFields = 0;
			goto l4;
		}
		if (fmt < 2) {
			fixedFields = totalLength;
			goto l4;
		}
		/* begin fetchClassOf: */
		if ((rcvr & 1)) {
			cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l5;
		}
		ccIndex = (((unsigned) (longAt(rcvr))) >> 12) & 31;
		if (ccIndex == 0) {
			cls = (longAt(rcvr - 4)) & 4294967292U;
			goto l5;
		} else {
			cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l5;
		}
	l5:	/* end fetchClassOf: */;
		classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
		fixedFields = (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
	l4:	/* end fixedFieldsOf:format:length: */;
		if ((fmt == 3) && (isContextHeader(hdr))) {
			/* begin fetchStackPointerOf: */
			sp1 = longAt(((((char *) rcvr)) + 4) + (2 << 2));
			if (!((sp1 & 1))) {
				sz = 0;
				goto l2;
			}
			sz = (sp1 >> 1);
		l2:	/* end fetchStackPointerOf: */;
			goto l1;
		} else {
			sz = totalLength - fixedFields;
			goto l1;
		}
	l1:	/* end stSizeOf: */;
	}
	if (successFlag) {
		/* begin pop:thenPush: */
		oop = positive32BitIntegerFor(sz);
		longAtput(sp = stackPointer - ((1 - 1) * 4), oop);
		stackPointer = sp;
	}
}

void primitiveSomeInstance(void) {
    int instance;
    int cls;
    int sp;
    int top;
    int thisClass;
    int thisObj;
    int ccIndex;
    int obj;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header;
    int extra2;
    int type1;
    int extra11;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	cls = top;
	/* begin initialInstanceOf: */
	/* begin firstAccessibleObject */
	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	obj = chunk + extra;
	while (obj < endOfMemory) {
		if (!(((longAt(obj)) & 3) == 2)) {
			thisObj = obj;
			goto l4;
		}
		/* begin objectAfter: */
		;
		if (((longAt(obj)) & 3) == 2) {
			sz = (longAt(obj)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header = longAt(obj);
			if ((header & 3) == 0) {
				sz = (longAt(obj - 8)) & 4294967292U;
				goto l3;
			} else {
				sz = header & 252;
				goto l3;
			}
		l3:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(obj + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		obj = (obj + sz) + extra2;
	}
	error("heap is empty");
l4:	/* end firstAccessibleObject */;
	while (!(thisObj == null)) {
		/* begin fetchClassOf: */
		if ((thisObj & 1)) {
			thisClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l2;
		}
		ccIndex = (((unsigned) (longAt(thisObj))) >> 12) & 31;
		if (ccIndex == 0) {
			thisClass = (longAt(thisObj - 4)) & 4294967292U;
			goto l2;
		} else {
			thisClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l2;
		}
	l2:	/* end fetchClassOf: */;
		if (thisClass == cls) {
			instance = thisObj;
			goto l1;
		}
		thisObj = accessibleObjectAfter(thisObj);
	}
	instance = nilObj;
l1:	/* end initialInstanceOf: */;
	if (instance == nilObj) {
		/* begin unPop: */
		stackPointer += 1 * 4;
		successFlag = 0;
	} else {
		/* begin push: */
		longAtput(sp = stackPointer + 4, instance);
		stackPointer = sp;
	}
}

void primitiveSomeObject(void) {
    int object;
    int sp;
    int obj;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header;
    int extra2;
    int type1;
    int extra11;

	/* begin pop: */
	stackPointer -= 1 * 4;
	/* begin push: */
	/* begin firstAccessibleObject */
	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	obj = chunk + extra;
	while (obj < endOfMemory) {
		if (!(((longAt(obj)) & 3) == 2)) {
			object = obj;
			goto l2;
		}
		/* begin objectAfter: */
		;
		if (((longAt(obj)) & 3) == 2) {
			sz = (longAt(obj)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header = longAt(obj);
			if ((header & 3) == 0) {
				sz = (longAt(obj - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(obj + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		obj = (obj + sz) + extra2;
	}
	error("heap is empty");
l2:	/* end firstAccessibleObject */;
	longAtput(sp = stackPointer + 4, object);
	stackPointer = sp;
}

void primitiveSoundStop(void) {
}

void primitiveSpecialObjectsOop(void) {
    int sp;

	/* begin pop: */
	stackPointer -= 1 * 4;
	/* begin push: */
	longAtput(sp = stackPointer + 4, specialObjectsOop);
	stackPointer = sp;
}

void primitiveSquareRoot(void) {
    double rcvr;

	rcvr = popFloat();
	successFlag = (rcvr >= 0.0) && successFlag;
	if (successFlag) {
		pushFloat(sqrt(rcvr));
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

int primitiveStoreStackp(void) {
    int stackp;
    int i;
    int newStackp;
    int ctxt;
    int valuePointer;
    int integerPointer;
    int sp;

	ctxt = longAt(stackPointer - (1 * 4));
	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (0 * 4));
	if ((integerPointer & 1)) {
		newStackp = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		newStackp = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	successFlag = (newStackp >= 0) && successFlag;
	successFlag = (newStackp <= ((((int) (156 - 4) >> 2)) - 6)) && successFlag;
	if (!(successFlag)) {
		return successFlag = 0;
	}
	/* begin fetchStackPointerOf: */
	sp = longAt(((((char *) ctxt)) + 4) + (2 << 2));
	if (!((sp & 1))) {
		stackp = 0;
		goto l2;
	}
	stackp = (sp >> 1);
l2:	/* end fetchStackPointerOf: */;
	if (newStackp > stackp) {
		for (i = (stackp + 1); i <= newStackp; i += 1) {
			/* begin storePointer:ofObject:withValue: */
			valuePointer = nilObj;
			if (ctxt < youngStart) {
				possibleRootStoreIntovalue(ctxt, valuePointer);
			}
			longAtput(((((char *) ctxt)) + 4) + (((i + 6) - 1) << 2), valuePointer);
		}
	}
	/* begin storeStackPointerValue:inContext: */
	longAtput(((((char *) ctxt)) + 4) + (2 << 2), ((newStackp << 1) | 1));
	/* begin pop: */
	stackPointer -= 1 * 4;
}

void primitiveStringAt(void) {
	commonAt(1);
}

void primitiveStringAtPut(void) {
	commonAtPut(1);
}

int primitiveStringReplace(void) {
    int array;
    int srcIndex;
    int start;
    int stop;
    int arrayFmt;
    int arrayInstSize;
    int i;
    int hdr;
    int totalLength;
    int repl;
    int replStart;
    int replFmt;
    int replInstSize;
    int integerPointer;
    int integerPointer1;
    int integerPointer2;
    int sz;
    int classFormat;
    int cls;
    int sz1;
    int classFormat1;
    int cls1;
    int ccIndex;
    int ccIndex1;

	array = longAt(stackPointer - (4 * 4));
	/* begin stackIntegerValue: */
	integerPointer = longAt(stackPointer - (3 * 4));
	if ((integerPointer & 1)) {
		start = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		start = 0;
		goto l1;
	}
l1:	/* end stackIntegerValue: */;
	/* begin stackIntegerValue: */
	integerPointer1 = longAt(stackPointer - (2 * 4));
	if ((integerPointer1 & 1)) {
		stop = (integerPointer1 >> 1);
		goto l2;
	} else {
		successFlag = 0;
		stop = 0;
		goto l2;
	}
l2:	/* end stackIntegerValue: */;
	repl = longAt(stackPointer - (1 * 4));
	/* begin stackIntegerValue: */
	integerPointer2 = longAt(stackPointer - (0 * 4));
	if ((integerPointer2 & 1)) {
		replStart = (integerPointer2 >> 1);
		goto l3;
	} else {
		successFlag = 0;
		replStart = 0;
		goto l3;
	}
l3:	/* end stackIntegerValue: */;
	if (!(successFlag)) {
		return successFlag = 0;
	}
	if ((repl & 1)) {
		return successFlag = 0;
	}
	hdr = longAt(array);
	arrayFmt = (((unsigned) hdr) >> 8) & 15;
	/* begin lengthOf:baseHeader:format: */
	if ((hdr & 3) == 0) {
		sz = (longAt(array - 8)) & 4294967292U;
	} else {
		sz = hdr & 252;
	}
	if (arrayFmt < 8) {
		totalLength = ((unsigned) (sz - 4)) >> 2;
		goto l4;
	} else {
		totalLength = (sz - 4) - (arrayFmt & 3);
		goto l4;
	}
l4:	/* end lengthOf:baseHeader:format: */;
	/* begin fixedFieldsOf:format:length: */
	if ((arrayFmt > 4) || (arrayFmt == 2)) {
		arrayInstSize = 0;
		goto l5;
	}
	if (arrayFmt < 2) {
		arrayInstSize = totalLength;
		goto l5;
	}
	/* begin fetchClassOf: */
	if ((array & 1)) {
		cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l8;
	}
	ccIndex = (((unsigned) (longAt(array))) >> 12) & 31;
	if (ccIndex == 0) {
		cls = (longAt(array - 4)) & 4294967292U;
		goto l8;
	} else {
		cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l8;
	}
l8:	/* end fetchClassOf: */;
	classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
	arrayInstSize = (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
l5:	/* end fixedFieldsOf:format:length: */;
	if (!((start >= 1) && ((start <= stop) && ((stop + arrayInstSize) <= totalLength)))) {
		return successFlag = 0;
	}
	hdr = longAt(repl);
	replFmt = (((unsigned) hdr) >> 8) & 15;
	/* begin lengthOf:baseHeader:format: */
	if ((hdr & 3) == 0) {
		sz1 = (longAt(repl - 8)) & 4294967292U;
	} else {
		sz1 = hdr & 252;
	}
	if (replFmt < 8) {
		totalLength = ((unsigned) (sz1 - 4)) >> 2;
		goto l6;
	} else {
		totalLength = (sz1 - 4) - (replFmt & 3);
		goto l6;
	}
l6:	/* end lengthOf:baseHeader:format: */;
	/* begin fixedFieldsOf:format:length: */
	if ((replFmt > 4) || (replFmt == 2)) {
		replInstSize = 0;
		goto l7;
	}
	if (replFmt < 2) {
		replInstSize = totalLength;
		goto l7;
	}
	/* begin fetchClassOf: */
	if ((repl & 1)) {
		cls1 = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l9;
	}
	ccIndex1 = (((unsigned) (longAt(repl))) >> 12) & 31;
	if (ccIndex1 == 0) {
		cls1 = (longAt(repl - 4)) & 4294967292U;
		goto l9;
	} else {
		cls1 = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex1 - 1) << 2));
		goto l9;
	}
l9:	/* end fetchClassOf: */;
	classFormat1 = (longAt(((((char *) cls1)) + 4) + (2 << 2))) - 1;
	replInstSize = (((((unsigned) classFormat1) >> 11) & 192) + ((((unsigned) classFormat1) >> 2) & 63)) - 1;
l7:	/* end fixedFieldsOf:format:length: */;
	if (!((replStart >= 1) && ((((stop - start) + replStart) + replInstSize) <= totalLength))) {
		return successFlag = 0;
	}
	if (arrayFmt < 8) {
		if (!(arrayFmt == replFmt)) {
			return successFlag = 0;
		}
	} else {
		if (!((arrayFmt & 12) == (replFmt & 12))) {
			return successFlag = 0;
		}
	}
	srcIndex = (replStart + replInstSize) - 1;
	if (arrayFmt < 4) {
		for (i = ((start + arrayInstSize) - 1); i <= ((stop + arrayInstSize) - 1); i += 1) {
			/* begin storePointer:ofObject:withValue: */
			if (array < youngStart) {
				possibleRootStoreIntovalue(array, longAt(((((char *) repl)) + 4) + (srcIndex << 2)));
			}
			longAtput(((((char *) array)) + 4) + (i << 2), longAt(((((char *) repl)) + 4) + (srcIndex << 2)));
			srcIndex += 1;
		}
	} else {
		if (arrayFmt < 8) {
			for (i = ((start + arrayInstSize) - 1); i <= ((stop + arrayInstSize) - 1); i += 1) {
				longAtput(((((char *) array)) + 4) + (i << 2), longAt(((((char *) repl)) + 4) + (srcIndex << 2)));
				srcIndex += 1;
			}
		} else {
			for (i = ((start + arrayInstSize) - 1); i <= ((stop + arrayInstSize) - 1); i += 1) {
				byteAtput(((((char *) array)) + 4) + i, byteAt(((((char *) repl)) + 4) + srcIndex));
				srcIndex += 1;
			}
		}
	}
	/* begin pop: */
	stackPointer -= 4 * 4;
}

void primitiveSubtract(void) {
    int integerResult;
    int sp;

	/* begin pop2AndPushIntegerIfOK: */
	integerResult = (stackIntegerValue(1)) - (stackIntegerValue(0));
	if (successFlag) {
		if ((integerResult ^ (integerResult << 1)) >= 0) {
			/* begin pop:thenPush: */
			longAtput(sp = stackPointer - ((2 - 1) * 4), ((integerResult << 1) | 1));
			stackPointer = sp;
		} else {
			successFlag = 0;
		}
	}
}

void primitiveSuspend(void) {
    int activeProc;
    int sp;
    int aProc;
    int sched;
    int newProc;
    int oldProc;
    int valuePointer;
    int tmp;

	activeProc = longAt(((((char *) (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2))))) + 4) + (1 << 2));
	successFlag = ((longAt(stackPointer)) == activeProc) && successFlag;
	if (successFlag) {
		/* begin pop: */
		stackPointer -= 1 * 4;
		/* begin push: */
		longAtput(sp = stackPointer + 4, nilObj);
		stackPointer = sp;
		/* begin transferTo: */
		aProc = wakeHighestPriority();
		newProc = aProc;
		sched = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2));
		oldProc = longAt(((((char *) sched)) + 4) + (1 << 2));
		/* begin storePointer:ofObject:withValue: */
		valuePointer = activeContext;
		if (oldProc < youngStart) {
			possibleRootStoreIntovalue(oldProc, valuePointer);
		}
		longAtput(((((char *) oldProc)) + 4) + (1 << 2), valuePointer);
		/* begin storePointer:ofObject:withValue: */
		if (sched < youngStart) {
			possibleRootStoreIntovalue(sched, newProc);
		}
		longAtput(((((char *) sched)) + 4) + (1 << 2), newProc);
		/* begin newActiveContext: */
		/* begin storeContextRegisters: */
		longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
		longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
		if ((longAt(((((char *) newProc)) + 4) + (1 << 2))) < youngStart) {
			beRootIfOld(longAt(((((char *) newProc)) + 4) + (1 << 2)));
		}
		activeContext = longAt(((((char *) newProc)) + 4) + (1 << 2));
		/* begin fetchContextRegisters: */
		tmp = longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (3 << 2));
		if ((tmp & 1)) {
			tmp = longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (5 << 2));
			if (tmp < youngStart) {
				beRootIfOld(tmp);
			}
		} else {
			tmp = longAt(((((char *) newProc)) + 4) + (1 << 2));
		}
		theHomeContext = tmp;
		receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
		method = longAt(((((char *) tmp)) + 4) + (3 << 2));
		tmp = ((longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (1 << 2))) >> 1);
		instructionPointer = ((method + tmp) + 4) - 2;
		tmp = ((longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (2 << 2))) >> 1);
		stackPointer = ((longAt(((((char *) newProc)) + 4) + (1 << 2))) + 4) + (((6 + tmp) - 1) * 4);
		reclaimableContextCount = 0;
	}
}

void primitiveTimesTwoPower(void) {
    int arg;
    double rcvr;
    int integerPointer;
    int top;

	/* begin popInteger */
	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	integerPointer = top;
	if ((integerPointer & 1)) {
		arg = (integerPointer >> 1);
		goto l1;
	} else {
		successFlag = 0;
		arg = 1;
		goto l1;
	}
l1:	/* end popInteger */;
	rcvr = popFloat();
	if (successFlag) {
		pushFloat(ldexp(rcvr, arg));
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveTruncated(void) {
    double trunc;
    double frac;
    double rcvr;

	rcvr = popFloat();
	if (successFlag) {
		frac = modf(rcvr, &trunc);
		success((-1073741824.0 <= trunc) && (trunc <= 1073741823.0));
	}
	if (successFlag) {
		pushInteger((int) trunc);
	} else {
		/* begin unPop: */
		stackPointer += 1 * 4;
	}
}

int primitiveVMParameter(void) {
    int mem;
    int arg;
    int paramsArraySize;
    int i;
    int index;
    int result;
    int sp;
    int sp1;
    int sp2;

	mem = ((int) memory);
	if (argumentCount == 0) {
		paramsArraySize = 22;
		result = instantiateClassindexableSize(longAt(((((char *) specialObjectsOop)) + 4) + (7 << 2)), paramsArraySize);
		for (i = 0; i <= (paramsArraySize - 1); i += 1) {
			longAtput(((((char *) result)) + 4) + (i << 2), ((0 << 1) | 1));
		}
		longAtput(((((char *) result)) + 4) + (0 << 2), (((youngStart - mem) << 1) | 1));
		longAtput(((((char *) result)) + 4) + (1 << 2), (((freeBlock - mem) << 1) | 1));
		longAtput(((((char *) result)) + 4) + (2 << 2), (((endOfMemory - mem) << 1) | 1));
		longAtput(((((char *) result)) + 4) + (3 << 2), ((allocationCount << 1) | 1));
		longAtput(((((char *) result)) + 4) + (4 << 2), ((allocationsBetweenGCs << 1) | 1));
		longAtput(((((char *) result)) + 4) + (5 << 2), ((tenuringThreshold << 1) | 1));
		longAtput(((((char *) result)) + 4) + (6 << 2), ((statFullGCs << 1) | 1));
		longAtput(((((char *) result)) + 4) + (7 << 2), ((statFullGCMSecs << 1) | 1));
		longAtput(((((char *) result)) + 4) + (8 << 2), ((statIncrGCs << 1) | 1));
		longAtput(((((char *) result)) + 4) + (9 << 2), ((statIncrGCMSecs << 1) | 1));
		longAtput(((((char *) result)) + 4) + (10 << 2), ((statTenures << 1) | 1));
		longAtput(((((char *) result)) + 4) + (20 << 2), ((rootTableCount << 1) | 1));
		longAtput(((((char *) result)) + 4) + (21 << 2), ((statRootTableOverflows << 1) | 1));
		/* begin pop:thenPush: */
		longAtput(sp = stackPointer - ((1 - 1) * 4), result);
		stackPointer = sp;
		return null;
	}
	arg = longAt(stackPointer);
	if (!((arg & 1))) {
		return successFlag = 0;
	}
	arg = (arg >> 1);
	if (argumentCount == 1) {
		if ((arg < 1) || (arg > 22)) {
			return successFlag = 0;
		}
		if (arg == 1) {
			result = youngStart - mem;
		}
		if (arg == 2) {
			result = freeBlock - mem;
		}
		if (arg == 3) {
			result = endOfMemory - mem;
		}
		if (arg == 4) {
			result = allocationCount;
		}
		if (arg == 5) {
			result = allocationsBetweenGCs;
		}
		if (arg == 6) {
			result = tenuringThreshold;
		}
		if (arg == 7) {
			result = statFullGCs;
		}
		if (arg == 8) {
			result = statFullGCMSecs;
		}
		if (arg == 9) {
			result = statIncrGCs;
		}
		if (arg == 10) {
			result = statIncrGCMSecs;
		}
		if (arg == 11) {
			result = statTenures;
		}
		if ((arg >= 12) && (arg <= 20)) {
			result = 0;
		}
		if (arg == 21) {
			result = rootTableCount;
		}
		if (arg == 22) {
			result = statRootTableOverflows;
		}
		/* begin pop:thenPush: */
		longAtput(sp1 = stackPointer - ((2 - 1) * 4), ((result << 1) | 1));
		stackPointer = sp1;
		return null;
	}
	if (!(argumentCount == 2)) {
		return successFlag = 0;
	}
	index = longAt(stackPointer - (1 * 4));
	if (!((index & 1))) {
		return successFlag = 0;
	}
	index = (index >> 1);
	if (index <= 0) {
		return successFlag = 0;
	}
	successFlag = 0;
	if (index == 5) {
		result = allocationsBetweenGCs;
		allocationsBetweenGCs = arg;
		successFlag = 1;
	}
	if (index == 6) {
		result = tenuringThreshold;
		tenuringThreshold = arg;
		successFlag = 1;
	}
	if (successFlag) {
		/* begin pop:thenPush: */
		longAtput(sp2 = stackPointer - ((3 - 1) * 4), ((result << 1) | 1));
		stackPointer = sp2;
		return null;
	}
	successFlag = 0;
}

void primitiveValue(void) {
    int blockArgumentCount;
    int initialIP;
    int blockContext;
    int toIndex;
    int fromIndex;
    int lastFrom;
    int successValue;
    int tmp;
    int argCount;

	blockContext = longAt(stackPointer - (argumentCount * 4));
	/* begin argumentCountOfBlock: */
	argCount = longAt(((((char *) blockContext)) + 4) + (3 << 2));
	if ((argCount & 1)) {
		blockArgumentCount = (argCount >> 1);
		goto l1;
	} else {
		successFlag = 0;
		blockArgumentCount = 0;
		goto l1;
	}
l1:	/* end argumentCountOfBlock: */;
	/* begin success: */
	successValue = (argumentCount == blockArgumentCount) && ((longAt(((((char *) blockContext)) + 4) + (0 << 2))) == nilObj);
	successFlag = successValue && successFlag;
	if (successFlag) {
		/* begin transfer:fromIndex:ofObject:toIndex:ofObject: */
		fromIndex = activeContext + ((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - argumentCount) + 1) * 4);
		toIndex = blockContext + (6 * 4);
		lastFrom = fromIndex + (argumentCount * 4);
		while (fromIndex < lastFrom) {
			fromIndex += 4;
			toIndex += 4;
			longAtput(toIndex, longAt(fromIndex));
		}
		/* begin pop: */
		stackPointer -= (argumentCount + 1) * 4;
		initialIP = longAt(((((char *) blockContext)) + 4) + (4 << 2));
		longAtput(((((char *) blockContext)) + 4) + (1 << 2), initialIP);
		/* begin storeStackPointerValue:inContext: */
		longAtput(((((char *) blockContext)) + 4) + (2 << 2), ((argumentCount << 1) | 1));
		longAtput(((((char *) blockContext)) + 4) + (0 << 2), activeContext);
		/* begin newActiveContext: */
		/* begin storeContextRegisters: */
		longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
		longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
		if (blockContext < youngStart) {
			beRootIfOld(blockContext);
		}
		activeContext = blockContext;
		/* begin fetchContextRegisters: */
		tmp = longAt(((((char *) blockContext)) + 4) + (3 << 2));
		if ((tmp & 1)) {
			tmp = longAt(((((char *) blockContext)) + 4) + (5 << 2));
			if (tmp < youngStart) {
				beRootIfOld(tmp);
			}
		} else {
			tmp = blockContext;
		}
		theHomeContext = tmp;
		receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
		method = longAt(((((char *) tmp)) + 4) + (3 << 2));
		tmp = ((longAt(((((char *) blockContext)) + 4) + (1 << 2))) >> 1);
		instructionPointer = ((method + tmp) + 4) - 2;
		tmp = ((longAt(((((char *) blockContext)) + 4) + (2 << 2))) >> 1);
		stackPointer = (blockContext + 4) + (((6 + tmp) - 1) * 4);
	}
}

void primitiveValueWithArgs(void) {
    int argumentArray;
    int arrayArgumentCount;
    int blockArgumentCount;
    int initialIP;
    int blockContext;
    int sz;
    int successValue;
    int toIndex;
    int fromIndex;
    int lastFrom;
    int top;
    int top1;
    int argCount;
    int header;
    int tmp;

	/* begin popStack */
	top = longAt(stackPointer);
	stackPointer -= 4;
	argumentArray = top;
	/* begin popStack */
	top1 = longAt(stackPointer);
	stackPointer -= 4;
	blockContext = top1;
	/* begin argumentCountOfBlock: */
	argCount = longAt(((((char *) blockContext)) + 4) + (3 << 2));
	if ((argCount & 1)) {
		blockArgumentCount = (argCount >> 1);
		goto l1;
	} else {
		successFlag = 0;
		blockArgumentCount = 0;
		goto l1;
	}
l1:	/* end argumentCountOfBlock: */;
	if (successFlag) {
		/* begin fetchWordLengthOf: */
		/* begin sizeBitsOf: */
		header = longAt(argumentArray);
		if ((header & 3) == 0) {
			sz = (longAt(argumentArray - 8)) & 4294967292U;
			goto l2;
		} else {
			sz = header & 252;
			goto l2;
		}
	l2:	/* end sizeBitsOf: */;
		arrayArgumentCount = ((unsigned) (sz - 4)) >> 2;
		/* begin success: */
		successValue = (arrayArgumentCount == blockArgumentCount) && ((longAt(((((char *) blockContext)) + 4) + (0 << 2))) == nilObj);
		successFlag = successValue && successFlag;
	}
	if (successFlag) {
		/* begin transfer:fromIndex:ofObject:toIndex:ofObject: */
		fromIndex = argumentArray + (0 * 4);
		toIndex = blockContext + (6 * 4);
		lastFrom = fromIndex + (arrayArgumentCount * 4);
		while (fromIndex < lastFrom) {
			fromIndex += 4;
			toIndex += 4;
			longAtput(toIndex, longAt(fromIndex));
		}
		initialIP = longAt(((((char *) blockContext)) + 4) + (4 << 2));
		longAtput(((((char *) blockContext)) + 4) + (1 << 2), initialIP);
		/* begin storeStackPointerValue:inContext: */
		longAtput(((((char *) blockContext)) + 4) + (2 << 2), ((arrayArgumentCount << 1) | 1));
		longAtput(((((char *) blockContext)) + 4) + (0 << 2), activeContext);
		/* begin newActiveContext: */
		/* begin storeContextRegisters: */
		longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
		longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
		if (blockContext < youngStart) {
			beRootIfOld(blockContext);
		}
		activeContext = blockContext;
		/* begin fetchContextRegisters: */
		tmp = longAt(((((char *) blockContext)) + 4) + (3 << 2));
		if ((tmp & 1)) {
			tmp = longAt(((((char *) blockContext)) + 4) + (5 << 2));
			if (tmp < youngStart) {
				beRootIfOld(tmp);
			}
		} else {
			tmp = blockContext;
		}
		theHomeContext = tmp;
		receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
		method = longAt(((((char *) tmp)) + 4) + (3 << 2));
		tmp = ((longAt(((((char *) blockContext)) + 4) + (1 << 2))) >> 1);
		instructionPointer = ((method + tmp) + 4) - 2;
		tmp = ((longAt(((((char *) blockContext)) + 4) + (2 << 2))) >> 1);
		stackPointer = (blockContext + 4) + (((6 + tmp) - 1) * 4);
	} else {
		/* begin unPop: */
		stackPointer += 2 * 4;
	}
}

void primitiveWait(void) {
    int sema;
    int activeProc;
    int excessSignals;
    int lastLink;
    int aProc;
    int sched;
    int newProc;
    int oldProc;
    int valuePointer;
    int tmp;

	sema = longAt(stackPointer);
	if (successFlag) {
		excessSignals = fetchIntegerofObject(2, sema);
		if (excessSignals > 0) {
			/* begin storeInteger:ofObject:withValue: */
			if (((excessSignals - 1) ^ ((excessSignals - 1) << 1)) >= 0) {
				longAtput(((((char *) sema)) + 4) + (2 << 2), (((excessSignals - 1) << 1) | 1));
			} else {
				successFlag = 0;
			}
		} else {
			activeProc = longAt(((((char *) (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2))))) + 4) + (1 << 2));
			/* begin addLastLink:toList: */
			if ((longAt(((((char *) sema)) + 4) + (0 << 2))) == nilObj) {
				/* begin storePointer:ofObject:withValue: */
				if (sema < youngStart) {
					possibleRootStoreIntovalue(sema, activeProc);
				}
				longAtput(((((char *) sema)) + 4) + (0 << 2), activeProc);
			} else {
				lastLink = longAt(((((char *) sema)) + 4) + (1 << 2));
				/* begin storePointer:ofObject:withValue: */
				if (lastLink < youngStart) {
					possibleRootStoreIntovalue(lastLink, activeProc);
				}
				longAtput(((((char *) lastLink)) + 4) + (0 << 2), activeProc);
			}
			/* begin storePointer:ofObject:withValue: */
			if (sema < youngStart) {
				possibleRootStoreIntovalue(sema, activeProc);
			}
			longAtput(((((char *) sema)) + 4) + (1 << 2), activeProc);
			/* begin storePointer:ofObject:withValue: */
			if (activeProc < youngStart) {
				possibleRootStoreIntovalue(activeProc, sema);
			}
			longAtput(((((char *) activeProc)) + 4) + (3 << 2), sema);
			/* begin transferTo: */
			aProc = wakeHighestPriority();
			newProc = aProc;
			sched = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2));
			oldProc = longAt(((((char *) sched)) + 4) + (1 << 2));
			/* begin storePointer:ofObject:withValue: */
			valuePointer = activeContext;
			if (oldProc < youngStart) {
				possibleRootStoreIntovalue(oldProc, valuePointer);
			}
			longAtput(((((char *) oldProc)) + 4) + (1 << 2), valuePointer);
			/* begin storePointer:ofObject:withValue: */
			if (sched < youngStart) {
				possibleRootStoreIntovalue(sched, newProc);
			}
			longAtput(((((char *) sched)) + 4) + (1 << 2), newProc);
			/* begin newActiveContext: */
			/* begin storeContextRegisters: */
			longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
			longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
			if ((longAt(((((char *) newProc)) + 4) + (1 << 2))) < youngStart) {
				beRootIfOld(longAt(((((char *) newProc)) + 4) + (1 << 2)));
			}
			activeContext = longAt(((((char *) newProc)) + 4) + (1 << 2));
			/* begin fetchContextRegisters: */
			tmp = longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (3 << 2));
			if ((tmp & 1)) {
				tmp = longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (5 << 2));
				if (tmp < youngStart) {
					beRootIfOld(tmp);
				}
			} else {
				tmp = longAt(((((char *) newProc)) + 4) + (1 << 2));
			}
			theHomeContext = tmp;
			receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
			method = longAt(((((char *) tmp)) + 4) + (3 << 2));
			tmp = ((longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (1 << 2))) >> 1);
			instructionPointer = ((method + tmp) + 4) - 2;
			tmp = ((longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (2 << 2))) >> 1);
			stackPointer = ((longAt(((((char *) newProc)) + 4) + (1 << 2))) + 4) + (((6 + tmp) - 1) * 4);
			reclaimableContextCount = 0;
		}
	}
}

void primitiveWarpBits(void) {
    int rcvr;
    int ns;
    int successValue;
    int skewWord;
    int mergeWord;
    int startBits;
    int yDelta;
    int smoothingCount;
    int sourceMapOop;
    int t;
    int i;
    int nSteps;
    int word;
    int halftoneWord;
    int deltaP12x;
    int deltaP12y;
    int deltaP43x;
    int deltaP43y;
    int pAx;
    int pAy;
    int pBx;
    int xDelta;
    int pBy;
    int integerPointer;

	rcvr = longAt(stackPointer - (argumentCount * 4));
	/* begin success: */
	successValue = loadBitBltFrom(rcvr);
	successFlag = successValue && successFlag;
	if (successFlag) {
		/* begin warpBits */
		ns = noSource;
		noSource = 1;
		clipRange();
		noSource = ns;
		if (noSource || ((bbW <= 0) || (bbH <= 0))) {
			affectedL = affectedR = affectedT = affectedB = 0;
			goto l1;
		}
		destMaskAndPointerInit();
		/* begin warpLoop */
		if (!((fetchWordLengthOf(bitBltOop)) >= (15 + 12))) {
			successFlag = 0;
			goto l3;
		}
		nSteps = height - 1;
		if (nSteps <= 0) {
			nSteps = 1;
		}
		pAx = fetchIntegerOrTruncFloatofObject(15, bitBltOop);
		t = fetchIntegerOrTruncFloatofObject(15 + 3, bitBltOop);
		deltaP12x = deltaFromtonSteps(pAx, t, nSteps);
		if (deltaP12x < 0) {
			pAx = t - (nSteps * deltaP12x);
		}
		pAy = fetchIntegerOrTruncFloatofObject(15 + 1, bitBltOop);
		t = fetchIntegerOrTruncFloatofObject(15 + 4, bitBltOop);
		deltaP12y = deltaFromtonSteps(pAy, t, nSteps);
		if (deltaP12y < 0) {
			pAy = t - (nSteps * deltaP12y);
		}
		pBx = fetchIntegerOrTruncFloatofObject(15 + 9, bitBltOop);
		t = fetchIntegerOrTruncFloatofObject(15 + 6, bitBltOop);
		deltaP43x = deltaFromtonSteps(pBx, t, nSteps);
		if (deltaP43x < 0) {
			pBx = t - (nSteps * deltaP43x);
		}
		pBy = fetchIntegerOrTruncFloatofObject(15 + 10, bitBltOop);
		t = fetchIntegerOrTruncFloatofObject(15 + 7, bitBltOop);
		deltaP43y = deltaFromtonSteps(pBy, t, nSteps);
		if (deltaP43y < 0) {
			pBy = t - (nSteps * deltaP43y);
		}
		if (!successFlag) {
			goto l3;
		}
		if (argumentCount == 2) {
			/* begin stackIntegerValue: */
			integerPointer = longAt(stackPointer - (1 * 4));
			if ((integerPointer & 1)) {
				smoothingCount = (integerPointer >> 1);
				goto l2;
			} else {
				successFlag = 0;
				smoothingCount = 0;
				goto l2;
			}
		l2:	/* end stackIntegerValue: */;
			sourceMapOop = longAt(stackPointer - (0 * 4));
			if (sourceMapOop == nilObj) {
				if (sourcePixSize < 16) {
					successFlag = 0;
					goto l3;
				}
			} else {
				if ((fetchWordLengthOf(sourceMapOop)) < (1 << sourcePixSize)) {
					successFlag = 0;
					goto l3;
				}
			}
		} else {
			smoothingCount = 1;
			sourceMapOop = nilObj;
		}
		startBits = pixPerWord - (dx & (pixPerWord - 1));
		nSteps = width - 1;
		if (nSteps <= 0) {
			nSteps = 1;
		}
		for (i = destY; i <= (clipY - 1); i += 1) {
			pAx += deltaP12x;
			pAy += deltaP12y;
			pBx += deltaP43x;
			pBy += deltaP43y;
		}
		for (i = 1; i <= bbH; i += 1) {
			xDelta = deltaFromtonSteps(pAx, pBx, nSteps);
			if (xDelta >= 0) {
				sx = pAx;
			} else {
				sx = pBx - (nSteps * xDelta);
			}
			yDelta = deltaFromtonSteps(pAy, pBy, nSteps);
			if (yDelta >= 0) {
				sy = pAy;
			} else {
				sy = pBy - (nSteps * yDelta);
			}
			for (word = destX; word <= (clipX - 1); word += 1) {
				sx += xDelta;
				sy += yDelta;
			}
			if (noHalftone) {
				halftoneWord = 4294967295U;
			} else {
				halftoneWord = longAt(halftoneBase + ((((dy + i) - 1) % halftoneHeight) * 4));
			}
			destMask = mask1;
			if (bbW < startBits) {
				skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(bbW, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
				skewWord = ((((startBits - bbW) * destPixSize) < 0) ? ((unsigned) skewWord >> -((startBits - bbW) * destPixSize)) : ((unsigned) skewWord << ((startBits - bbW) * destPixSize)));
			} else {
				skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(startBits, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
			}
			for (word = 1; word <= nWords; word += 1) {
				mergeWord = mergewith(skewWord & halftoneWord, (longAt(destIndex)) & destMask);
				longAtput(destIndex, (destMask & mergeWord) | ((~destMask) & (longAt(destIndex))));
				destIndex += 4;
				if (word >= (nWords - 1)) {
					if (!(word == nWords)) {
						destMask = mask2;
						skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(pixPerWord, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
					}
				} else {
					destMask = 4294967295U;
					skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(pixPerWord, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
				}
			}
			pAx += deltaP12x;
			pAy += deltaP12y;
			pBx += deltaP43x;
			pBy += deltaP43y;
			destIndex += destDelta;
		}
	l3:	/* end warpLoop */;
		if (hDir > 0) {
			affectedL = dx;
			affectedR = dx + bbW;
		} else {
			affectedL = (dx - bbW) + 1;
			affectedR = dx + 1;
		}
		if (vDir > 0) {
			affectedT = dy;
			affectedB = dy + bbH;
		} else {
			affectedT = (dy - bbH) + 1;
			affectedB = dy + 1;
		}
	l1:	/* end warpBits */;
		showDisplayBits();
	}
}

void print(const char *s) {
    int i;
    int ch;

	i = 0;
	while (!((ch = s[i]) == 0)) {
		ioPutChar(ch);
		i += 1;
	}
}

void printCallStack(void) {
	printCallStackFrom(activeContext);
}

void printCallStackFrom(int aContext) {
    int methodSel;
    int methodClass;
    int home;
    int ctxt;
    int methodArray;
    int done;
    int i;
    int classDict;
    int currClass;
    int classDictSize;
    int sz;
    int header;
    int ccIndex;
    int ccIndex1;
    int methodArray1;
    int done1;
    int i1;
    int classDict1;
    int currClass1;
    int classDictSize1;
    int sz1;
    int header1;
    int ccIndex2;

	ctxt = aContext;
	while (!(ctxt == nilObj)) {
		if ((fetchClassOf(ctxt)) == (longAt(((((char *) specialObjectsOop)) + 4) + (11 << 2)))) {
			home = longAt(((((char *) ctxt)) + 4) + (5 << 2));
		} else {
			home = ctxt;
		}
		/* begin findClassOfMethod:forReceiver: */
		/* begin fetchClassOf: */
		if (((longAt(((((char *) home)) + 4) + (5 << 2))) & 1)) {
			currClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l2;
		}
		ccIndex = (((unsigned) (longAt(longAt(((((char *) home)) + 4) + (5 << 2))))) >> 12) & 31;
		if (ccIndex == 0) {
			currClass = (longAt((longAt(((((char *) home)) + 4) + (5 << 2))) - 4)) & 4294967292U;
			goto l2;
		} else {
			currClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
			goto l2;
		}
	l2:	/* end fetchClassOf: */;
		done = 0;
		while (!(done)) {
			classDict = longAt(((((char *) currClass)) + 4) + (1 << 2));
			/* begin fetchWordLengthOf: */
			/* begin sizeBitsOf: */
			header = longAt(classDict);
			if ((header & 3) == 0) {
				sz = (longAt(classDict - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
			classDictSize = ((unsigned) (sz - 4)) >> 2;
			methodArray = longAt(((((char *) classDict)) + 4) + (1 << 2));
			i = 0;
			while (i < (classDictSize - 2)) {
				if ((longAt(((((char *) home)) + 4) + (3 << 2))) == (longAt(((((char *) methodArray)) + 4) + (i << 2)))) {
					methodClass = currClass;
					goto l3;
				}
				i += 1;
			}
			currClass = longAt(((((char *) currClass)) + 4) + (0 << 2));
			done = currClass == nilObj;
		}
		/* begin fetchClassOf: */
		if (((longAt(((((char *) home)) + 4) + (5 << 2))) & 1)) {
			methodClass = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l3;
		}
		ccIndex1 = (((unsigned) (longAt(longAt(((((char *) home)) + 4) + (5 << 2))))) >> 12) & 31;
		if (ccIndex1 == 0) {
			methodClass = (longAt((longAt(((((char *) home)) + 4) + (5 << 2))) - 4)) & 4294967292U;
			goto l3;
		} else {
			methodClass = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex1 - 1) << 2));
			goto l3;
		}
		methodClass = null;
	l3:	/* end findClassOfMethod:forReceiver: */;
		/* begin findSelectorOfMethod:forReceiver: */
		/* begin fetchClassOf: */
		if (((longAt(((((char *) home)) + 4) + (5 << 2))) & 1)) {
			currClass1 = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
			goto l5;
		}
		ccIndex2 = (((unsigned) (longAt(longAt(((((char *) home)) + 4) + (5 << 2))))) >> 12) & 31;
		if (ccIndex2 == 0) {
			currClass1 = (longAt((longAt(((((char *) home)) + 4) + (5 << 2))) - 4)) & 4294967292U;
			goto l5;
		} else {
			currClass1 = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex2 - 1) << 2));
			goto l5;
		}
	l5:	/* end fetchClassOf: */;
		done1 = 0;
		while (!(done1)) {
			classDict1 = longAt(((((char *) currClass1)) + 4) + (1 << 2));
			/* begin fetchWordLengthOf: */
			/* begin sizeBitsOf: */
			header1 = longAt(classDict1);
			if ((header1 & 3) == 0) {
				sz1 = (longAt(classDict1 - 8)) & 4294967292U;
				goto l4;
			} else {
				sz1 = header1 & 252;
				goto l4;
			}
		l4:	/* end sizeBitsOf: */;
			classDictSize1 = ((unsigned) (sz1 - 4)) >> 2;
			methodArray1 = longAt(((((char *) classDict1)) + 4) + (1 << 2));
			i1 = 0;
			while (i1 <= (classDictSize1 - 2)) {
				if ((longAt(((((char *) home)) + 4) + (3 << 2))) == (longAt(((((char *) methodArray1)) + 4) + (i1 << 2)))) {
					methodSel = longAt(((((char *) classDict1)) + 4) + ((i1 + 2) << 2));
					goto l6;
				}
				i1 += 1;
			}
			currClass1 = longAt(((((char *) currClass1)) + 4) + (0 << 2));
			done1 = currClass1 == nilObj;
		}
		methodSel = longAt(((((char *) specialObjectsOop)) + 4) + (20 << 2));
	l6:	/* end findSelectorOfMethod:forReceiver: */;
		if (!(ctxt == home)) {
			print("[] in ");
		}
		printNameOfClasscount(methodClass, 5);
		print(">");
		printStringOf(methodSel);
		ioPutChar(13);
		ctxt = longAt(((((char *) ctxt)) + 4) + (0 << 2));
	}
}

void printNameOfClasscount(int classOop, int cnt) {
	if (cnt <= 0) {
		print("bad class");
	} else {
		if ((sizeBitsOf(classOop)) == 32) {
			printNameOfClasscount(longAt(((((char *) classOop)) + 4) + (6 << 2)), cnt - 1);
			print(" class");
		} else {
			printStringOf(longAt(((((char *) classOop)) + 4) + (6 << 2)));
		}
	}
}

int printStringOf(int oop) {
    int i;
    int fmt;
    int cnt;

	fmt = (((unsigned) (longAt(oop))) >> 8) & 15;
	if (fmt < 8) {
		return null;
	}
	cnt = ((100 < (lengthOf(oop))) ? 100 : (lengthOf(oop)));
	i = 0;
	while (i < cnt) {
		ioPutChar(byteAt(((((char *) oop)) + 4) + i));
		i += 1;
	}
}

void push(int object) {
    int sp;

	longAtput(sp = stackPointer + 4, object);
	stackPointer = sp;
}

int pushBool(int trueOrFalse) {
    int sp;
    int sp1;

	if (trueOrFalse) {
		/* begin push: */
		longAtput(sp = stackPointer + 4, trueObj);
		stackPointer = sp;
	} else {
		/* begin push: */
		longAtput(sp1 = stackPointer + 4, falseObj);
		stackPointer = sp1;
	}
	return 0;
}

void pushFloat(double f) {
    int newFloatObj;
    int sp;

	newFloatObj = instantiateSmallClasssizeInBytesfill(longAt(((((char *) specialObjectsOop)) + 4) + (9 << 2)), 12, 0);
	storeFloatAtfrom(newFloatObj + 4, f);
	/* begin push: */
	longAtput(sp = stackPointer + 4, newFloatObj);
	stackPointer = sp;
}

void pushInteger(int integerValue) {
    int sp;

	/* begin push: */
	longAtput(sp = stackPointer + 4, ((integerValue << 1) | 1));
	stackPointer = sp;
}

void pushRemappableOop(int oop) {
	remapBuffer[remapBufferCount += 1] = oop;
}

void putToSleep(int aProcess) {
    int priority;
    int processLists;
    int processList;
    int lastLink;

	priority = ((longAt(((((char *) aProcess)) + 4) + (2 << 2))) >> 1);
	processLists = longAt(((((char *) (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2))))) + 4) + (0 << 2));
	processList = longAt(((((char *) processLists)) + 4) + ((priority - 1) << 2));
	/* begin addLastLink:toList: */
	if ((longAt(((((char *) processList)) + 4) + (0 << 2))) == nilObj) {
		/* begin storePointer:ofObject:withValue: */
		if (processList < youngStart) {
			possibleRootStoreIntovalue(processList, aProcess);
		}
		longAtput(((((char *) processList)) + 4) + (0 << 2), aProcess);
	} else {
		lastLink = longAt(((((char *) processList)) + 4) + (1 << 2));
		/* begin storePointer:ofObject:withValue: */
		if (lastLink < youngStart) {
			possibleRootStoreIntovalue(lastLink, aProcess);
		}
		longAtput(((((char *) lastLink)) + 4) + (0 << 2), aProcess);
	}
	/* begin storePointer:ofObject:withValue: */
	if (processList < youngStart) {
		possibleRootStoreIntovalue(processList, aProcess);
	}
	longAtput(((((char *) processList)) + 4) + (1 << 2), aProcess);
	/* begin storePointer:ofObject:withValue: */
	if (aProcess < youngStart) {
		possibleRootStoreIntovalue(aProcess, processList);
	}
	longAtput(((((char *) aProcess)) + 4) + (3 << 2), processList);
}

void quickCheckForInterrupts(void) {
	if ((interruptCheckCounter -= 1) <= 0) {
		checkForInterrupts();
	}
}

int quickFetchIntegerofObject(int fieldIndex, int objectPointer) {
	return ((longAt(((((char *) objectPointer)) + 4) + (fieldIndex << 2))) >> 1);
}

int readImageFromFileHeapSize(int f, int desiredHeapSize) {
    int swapBytes;
    int dataSize;
    int minimumMemory;
    int memStart;
    int bytesRead;
    int bytesToShift;
    int headerStart;
    int headerSize;
    int oldBaseAddr;
    int i;
    int startAddr;
    int addr;
    int methodHeader;
    int wordAddr;
    int oop;
    int fmt;
    int stopAddr;
    int addr1;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header;
    int extra2;
    int type1;
    int extra11;
    int sched;
    int proc;
    int activeCntx;
    int tmp;

	swapBytes = checkImageVersionFrom(f);
	headerStart = (sqImageFilePosition(f)) - 4;
	headerSize = getLongFromFileswap(f, swapBytes);
	dataSize = getLongFromFileswap(f, swapBytes);
	oldBaseAddr = getLongFromFileswap(f, swapBytes);
	specialObjectsOop = getLongFromFileswap(f, swapBytes);
	lastHash = getLongFromFileswap(f, swapBytes);
	savedWindowSize = getLongFromFileswap(f, swapBytes);
	fullScreenFlag = getLongFromFileswap(f, swapBytes);
	if (lastHash == 0) {
		lastHash = 999;
	}
	minimumMemory = dataSize + 80000;
	if (desiredHeapSize < minimumMemory) {
		error("Insufficient memory for this image");
	}
	memory = (unsigned char *) sqAllocateMemory(minimumMemory, desiredHeapSize);
	if (memory == null) {
		error("Failed to allocate memory for the heap");
	}
	memStart = startOfMemory();
	memoryLimit = (memStart + desiredHeapSize) - 24;
	endOfMemory = memStart + dataSize;
	sqImageFileSeek(f, headerStart + headerSize);
	bytesRead = sqImageFileRead(memory, sizeof(unsigned char), dataSize, f);
	if (bytesRead != dataSize) {
		error("Read failed or premature end of image file");
	}
	if (swapBytes) {
		/* begin reverseBytesInImage */
		/* begin reverseBytesFrom:to: */
		startAddr = startOfMemory();
		addr = startAddr;
		while (addr < endOfMemory) {
			longAtput(addr, ((((((unsigned) (longAt(addr)) >> 24)) & 255) + ((((unsigned) (longAt(addr)) >> 8)) & 65280)) + ((((unsigned) (longAt(addr)) << 8)) & 16711680)) + ((((unsigned) (longAt(addr)) << 24)) & 4278190080U));
			addr += 4;
		}
		/* begin byteSwapByteObjects */
		/* begin oopFromChunk: */
		chunk = startOfMemory();
		/* begin extraHeaderBytes: */
		type = (longAt(chunk)) & 3;
		if (type > 1) {
			extra1 = 0;
		} else {
			if (type == 1) {
				extra1 = 4;
			} else {
				extra1 = 8;
			}
		}
		extra = extra1;
		oop = chunk + extra;
		while (oop < endOfMemory) {
			if (!(((longAt(oop)) & 3) == 2)) {
				fmt = (((unsigned) (longAt(oop))) >> 8) & 15;
				if (fmt >= 8) {
					wordAddr = oop + 4;
					if (fmt >= 12) {
						methodHeader = longAt(oop + 4);
						wordAddr = (wordAddr + 4) + (((((unsigned) methodHeader) >> 10) & 255) * 4);
					}
					/* begin reverseBytesFrom:to: */
					stopAddr = oop + (sizeBitsOf(oop));
					addr1 = wordAddr;
					while (addr1 < stopAddr) {
						longAtput(addr1, ((((((unsigned) (longAt(addr1)) >> 24)) & 255) + ((((unsigned) (longAt(addr1)) >> 8)) & 65280)) + ((((unsigned) (longAt(addr1)) << 8)) & 16711680)) + ((((unsigned) (longAt(addr1)) << 24)) & 4278190080U));
						addr1 += 4;
					}
				}
			}
			/* begin objectAfter: */
			;
			if (((longAt(oop)) & 3) == 2) {
				sz = (longAt(oop)) & 4294967292U;
			} else {
				/* begin sizeBitsOf: */
				header = longAt(oop);
				if ((header & 3) == 0) {
					sz = (longAt(oop - 8)) & 4294967292U;
					goto l1;
				} else {
					sz = header & 252;
					goto l1;
				}
			l1:	/* end sizeBitsOf: */;
			}
			/* begin oopFromChunk: */
			/* begin extraHeaderBytes: */
			type1 = (longAt(oop + sz)) & 3;
			if (type1 > 1) {
				extra11 = 0;
			} else {
				if (type1 == 1) {
					extra11 = 4;
				} else {
					extra11 = 8;
				}
			}
			extra2 = extra11;
			oop = (oop + sz) + extra2;
		}
	}
	bytesToShift = memStart - oldBaseAddr;
	/* begin initializeInterpreter: */
	initializeObjectMemory(bytesToShift);
	initBBOpTable();
	activeContext = nilObj;
	theHomeContext = nilObj;
	method = nilObj;
	receiver = nilObj;
	messageSelector = nilObj;
	newMethod = nilObj;
	/* begin flushMethodCache */
	for (i = 1; i <= 1024; i += 1) {
		methodCache[i] = 0;
	}
	for (i = 1; i <= 64; i += 1) {
		atCache[i] = 0;
	}
	/* begin loadInitialContext */
	sched = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2));
	proc = longAt(((((char *) sched)) + 4) + (1 << 2));
	activeContext = longAt(((((char *) proc)) + 4) + (1 << 2));
	if (activeContext < youngStart) {
		beRootIfOld(activeContext);
	}
	/* begin fetchContextRegisters: */
	activeCntx = activeContext;
	tmp = longAt(((((char *) activeCntx)) + 4) + (3 << 2));
	if ((tmp & 1)) {
		tmp = longAt(((((char *) activeCntx)) + 4) + (5 << 2));
		if (tmp < youngStart) {
			beRootIfOld(tmp);
		}
	} else {
		tmp = activeCntx;
	}
	theHomeContext = tmp;
	receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
	method = longAt(((((char *) tmp)) + 4) + (3 << 2));
	tmp = ((longAt(((((char *) activeCntx)) + 4) + (1 << 2))) >> 1);
	instructionPointer = ((method + tmp) + 4) - 2;
	tmp = ((longAt(((((char *) activeCntx)) + 4) + (2 << 2))) >> 1);
	stackPointer = (activeCntx + 4) + (((6 + tmp) - 1) * 4);
	reclaimableContextCount = 0;
	interruptCheckCounter = 0;
	nextPollTick = 0;
	nextWakeupTick = 0;
	lastTick = 0;
	interruptKeycode = 2094;
	interruptPending = 0;
	semaphoresToSignalCount = 0;
	deferDisplayUpdates = 0;
	pendingFinalizationSignals = 0;
	return dataSize;
}

int readableFormat(int imageVersion) {
	return (imageVersion == 6502) || (imageVersion == 6504);
}

void recycleContextIfPossible(int cntxOop) {
	if ((cntxOop >= youngStart) && (((((unsigned) (longAt(cntxOop))) >> 12) & 31) == 14)) {
		longAtput(((((char *) cntxOop)) + 4) + (0 << 2), freeContexts);
		freeContexts = cntxOop;
	}
}

int remap(int oop) {
    int fwdBlock;

	if (((oop & 1) == 0) && (((longAt(oop)) & 2147483648U) != 0)) {
		fwdBlock = ((longAt(oop)) & 2147483644) << 1;
		;
		return longAt(fwdBlock);
	}
	return oop;
}

int remapClassOf(int oop) {
    int newClassOop;
    int fwdBlock;
    int classHeader;
    int classOop;
    int newClassHeader;

	if (((longAt(oop)) & 3) == 3) {
		return null;
	}
	classHeader = longAt(oop - 4);
	classOop = classHeader & 4294967292U;
	if (((classOop & 1) == 0) && (((longAt(classOop)) & 2147483648U) != 0)) {
		fwdBlock = ((longAt(classOop)) & 2147483644) << 1;
		;
		newClassOop = longAt(fwdBlock);
		newClassHeader = newClassOop | (classHeader & 3);
		longAtput(oop - 4, newClassHeader);
		if ((oop < youngStart) && (newClassOop >= youngStart)) {
			beRootWhileForwarding(oop);
		}
	}
}

void remapFieldsAndClassOf(int oop) {
    int fwdBlock;
    int fieldOffset;
    int fieldOop;
    int newOop;
    int methodHeader;
    int size;
    int fwdBlock1;
    int fmt;
    int header;
    int newClassOop;
    int fwdBlock2;
    int classHeader;
    int classOop;
    int newClassHeader;

	/* begin lastPointerWhileForwarding: */
	header = longAt(oop);
	if ((header & 2147483648U) != 0) {
		fwdBlock1 = (header & 2147483644) << 1;
		;
		header = longAt(fwdBlock1 + 4);
	}
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			fieldOffset = (6 + (fetchStackPointerOf(oop))) * 4;
			goto l1;
		}
		if ((header & 3) == 0) {
			size = (longAt(oop - 8)) & 4294967292U;
		} else {
			size = header & 252;
		}
		fieldOffset = size - 4;
		goto l1;
	}
	if (fmt < 12) {
		fieldOffset = 0;
		goto l1;
	}
	methodHeader = longAt(oop + 4);
	fieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
l1:	/* end lastPointerWhileForwarding: */;
	while (fieldOffset >= 4) {
		fieldOop = longAt(oop + fieldOffset);
		if (((fieldOop & 1) == 0) && (((longAt(fieldOop)) & 2147483648U) != 0)) {
			fwdBlock = ((longAt(fieldOop)) & 2147483644) << 1;
			;
			newOop = longAt(fwdBlock);
			longAtput(oop + fieldOffset, newOop);
			if ((oop < youngStart) && (newOop >= youngStart)) {
				beRootWhileForwarding(oop);
			}
		}
		fieldOffset -= 4;
	}
	/* begin remapClassOf: */
	if (((longAt(oop)) & 3) == 3) {
		goto l2;
	}
	classHeader = longAt(oop - 4);
	classOop = classHeader & 4294967292U;
	if (((classOop & 1) == 0) && (((longAt(classOop)) & 2147483648U) != 0)) {
		fwdBlock2 = ((longAt(classOop)) & 2147483644) << 1;
		;
		newClassOop = longAt(fwdBlock2);
		newClassHeader = newClassOop | (classHeader & 3);
		longAtput(oop - 4, newClassHeader);
		if ((oop < youngStart) && (newClassOop >= youngStart)) {
			beRootWhileForwarding(oop);
		}
	}
l2:	/* end remapClassOf: */;
}

int removeFirstLinkOfList(int aList) {
    int next;
    int first;
    int last;
    int valuePointer;
    int valuePointer1;
    int valuePointer2;

	first = longAt(((((char *) aList)) + 4) + (0 << 2));
	last = longAt(((((char *) aList)) + 4) + (1 << 2));
	if (first == last) {
		/* begin storePointer:ofObject:withValue: */
		valuePointer = nilObj;
		if (aList < youngStart) {
			possibleRootStoreIntovalue(aList, valuePointer);
		}
		longAtput(((((char *) aList)) + 4) + (0 << 2), valuePointer);
		/* begin storePointer:ofObject:withValue: */
		valuePointer1 = nilObj;
		if (aList < youngStart) {
			possibleRootStoreIntovalue(aList, valuePointer1);
		}
		longAtput(((((char *) aList)) + 4) + (1 << 2), valuePointer1);
	} else {
		next = longAt(((((char *) first)) + 4) + (0 << 2));
		/* begin storePointer:ofObject:withValue: */
		if (aList < youngStart) {
			possibleRootStoreIntovalue(aList, next);
		}
		longAtput(((((char *) aList)) + 4) + (0 << 2), next);
	}
	/* begin storePointer:ofObject:withValue: */
	valuePointer2 = nilObj;
	if (first < youngStart) {
		possibleRootStoreIntovalue(first, valuePointer2);
	}
	longAtput(((((char *) first)) + 4) + (0 << 2), valuePointer2);
	return first;
}

void restoreHeaderOf(int oop) {
    int fwdBlock;
    int fwdHeader;

	fwdHeader = longAt(oop);
	fwdBlock = (fwdHeader & 2147483644) << 1;
	;
	longAtput(oop, longAt(fwdBlock + 4));
}

void restoreHeadersAfterBecomingwith(int list1, int list2) {
    int fieldOffset;
    int oop1;
    int oop2;
    int hdr1;
    int hdr2;
    int fwdBlock;
    int fwdHeader;
    int fwdBlock1;
    int fwdHeader1;
    int methodHeader;
    int sz;
    int fmt;
    int header;
    int header1;
    int type;

	/* begin lastPointerOf: */
	header = longAt(list1);
	fmt = (((unsigned) header) >> 8) & 15;
	if (fmt <= 4) {
		if ((fmt == 3) && (isContextHeader(header))) {
			fieldOffset = (6 + (fetchStackPointerOf(list1))) * 4;
			goto l1;
		}
		/* begin sizeBitsOfSafe: */
		header1 = longAt(list1);
		/* begin rightType: */
		if ((header1 & 252) == 0) {
			type = 0;
			goto l2;
		} else {
			if ((header1 & 126976) == 0) {
				type = 1;
				goto l2;
			} else {
				type = 3;
				goto l2;
			}
		}
	l2:	/* end rightType: */;
		if (type == 0) {
			sz = (longAt(list1 - 8)) & 4294967292U;
			goto l3;
		} else {
			sz = header1 & 252;
			goto l3;
		}
	l3:	/* end sizeBitsOfSafe: */;
		fieldOffset = sz - 4;
		goto l1;
	}
	if (fmt < 12) {
		fieldOffset = 0;
		goto l1;
	}
	methodHeader = longAt(list1 + 4);
	fieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
l1:	/* end lastPointerOf: */;
	while (fieldOffset >= 4) {
		oop1 = longAt(list1 + fieldOffset);
		oop2 = longAt(list2 + fieldOffset);
		/* begin restoreHeaderOf: */
		fwdHeader = longAt(oop1);
		fwdBlock = (fwdHeader & 2147483644) << 1;
		;
		longAtput(oop1, longAt(fwdBlock + 4));
		/* begin restoreHeaderOf: */
		fwdHeader1 = longAt(oop2);
		fwdBlock1 = (fwdHeader1 & 2147483644) << 1;
		;
		longAtput(oop2, longAt(fwdBlock1 + 4));
		/* begin exchangeHashBits:with: */
		hdr1 = longAt(oop1);
		hdr2 = longAt(oop2);
		longAtput(oop1, (hdr1 & 3758227455U) | (hdr2 & 536739840));
		longAtput(oop2, (hdr2 & 3758227455U) | (hdr1 & 536739840));
		fieldOffset -= 4;
	}
}

void restoreHeadersAfterForwardBecome(void) {
    int fwdBlock;
    int oop1;
    int fwdBlock1;
    int fwdHeader;

	fwdBlock = ((endOfMemory + 4) + 7) & 4294967288U;
	fwdBlock += 16;
	while (fwdBlock <= fwdTableNext) {
		oop1 = longAt(fwdBlock + 8);
		/* begin restoreHeaderOf: */
		fwdHeader = longAt(oop1);
		fwdBlock1 = (fwdHeader & 2147483644) << 1;
		;
		longAtput(oop1, longAt(fwdBlock1 + 4));
		fwdBlock += 16;
	}
}

void resume(int aProcess) {
    int activeProc;
    int activePriority;
    int newPriority;
    int priority;
    int processLists;
    int processList;
    int lastLink;
    int sched;
    int newProc;
    int oldProc;
    int valuePointer;
    int tmp;
    int priority1;
    int processLists1;
    int processList1;
    int lastLink1;

	activeProc = longAt(((((char *) (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2))))) + 4) + (1 << 2));
	activePriority = ((longAt(((((char *) activeProc)) + 4) + (2 << 2))) >> 1);
	newPriority = ((longAt(((((char *) aProcess)) + 4) + (2 << 2))) >> 1);
	if (newPriority > activePriority) {
		/* begin putToSleep: */
		priority = ((longAt(((((char *) activeProc)) + 4) + (2 << 2))) >> 1);
		processLists = longAt(((((char *) (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2))))) + 4) + (0 << 2));
		processList = longAt(((((char *) processLists)) + 4) + ((priority - 1) << 2));
		/* begin addLastLink:toList: */
		if ((longAt(((((char *) processList)) + 4) + (0 << 2))) == nilObj) {
			/* begin storePointer:ofObject:withValue: */
			if (processList < youngStart) {
				possibleRootStoreIntovalue(processList, activeProc);
			}
			longAtput(((((char *) processList)) + 4) + (0 << 2), activeProc);
		} else {
			lastLink = longAt(((((char *) processList)) + 4) + (1 << 2));
			/* begin storePointer:ofObject:withValue: */
			if (lastLink < youngStart) {
				possibleRootStoreIntovalue(lastLink, activeProc);
			}
			longAtput(((((char *) lastLink)) + 4) + (0 << 2), activeProc);
		}
		/* begin storePointer:ofObject:withValue: */
		if (processList < youngStart) {
			possibleRootStoreIntovalue(processList, activeProc);
		}
		longAtput(((((char *) processList)) + 4) + (1 << 2), activeProc);
		/* begin storePointer:ofObject:withValue: */
		if (activeProc < youngStart) {
			possibleRootStoreIntovalue(activeProc, processList);
		}
		longAtput(((((char *) activeProc)) + 4) + (3 << 2), processList);
		/* begin transferTo: */
		newProc = aProcess;
		sched = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2));
		oldProc = longAt(((((char *) sched)) + 4) + (1 << 2));
		/* begin storePointer:ofObject:withValue: */
		valuePointer = activeContext;
		if (oldProc < youngStart) {
			possibleRootStoreIntovalue(oldProc, valuePointer);
		}
		longAtput(((((char *) oldProc)) + 4) + (1 << 2), valuePointer);
		/* begin storePointer:ofObject:withValue: */
		if (sched < youngStart) {
			possibleRootStoreIntovalue(sched, newProc);
		}
		longAtput(((((char *) sched)) + 4) + (1 << 2), newProc);
		/* begin newActiveContext: */
		/* begin storeContextRegisters: */
		longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
		longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
		if ((longAt(((((char *) newProc)) + 4) + (1 << 2))) < youngStart) {
			beRootIfOld(longAt(((((char *) newProc)) + 4) + (1 << 2)));
		}
		activeContext = longAt(((((char *) newProc)) + 4) + (1 << 2));
		/* begin fetchContextRegisters: */
		tmp = longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (3 << 2));
		if ((tmp & 1)) {
			tmp = longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (5 << 2));
			if (tmp < youngStart) {
				beRootIfOld(tmp);
			}
		} else {
			tmp = longAt(((((char *) newProc)) + 4) + (1 << 2));
		}
		theHomeContext = tmp;
		receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
		method = longAt(((((char *) tmp)) + 4) + (3 << 2));
		tmp = ((longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (1 << 2))) >> 1);
		instructionPointer = ((method + tmp) + 4) - 2;
		tmp = ((longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (2 << 2))) >> 1);
		stackPointer = ((longAt(((((char *) newProc)) + 4) + (1 << 2))) + 4) + (((6 + tmp) - 1) * 4);
		reclaimableContextCount = 0;
	} else {
		/* begin putToSleep: */
		priority1 = ((longAt(((((char *) aProcess)) + 4) + (2 << 2))) >> 1);
		processLists1 = longAt(((((char *) (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2))))) + 4) + (0 << 2));
		processList1 = longAt(((((char *) processLists1)) + 4) + ((priority1 - 1) << 2));
		/* begin addLastLink:toList: */
		if ((longAt(((((char *) processList1)) + 4) + (0 << 2))) == nilObj) {
			/* begin storePointer:ofObject:withValue: */
			if (processList1 < youngStart) {
				possibleRootStoreIntovalue(processList1, aProcess);
			}
			longAtput(((((char *) processList1)) + 4) + (0 << 2), aProcess);
		} else {
			lastLink1 = longAt(((((char *) processList1)) + 4) + (1 << 2));
			/* begin storePointer:ofObject:withValue: */
			if (lastLink1 < youngStart) {
				possibleRootStoreIntovalue(lastLink1, aProcess);
			}
			longAtput(((((char *) lastLink1)) + 4) + (0 << 2), aProcess);
		}
		/* begin storePointer:ofObject:withValue: */
		if (processList1 < youngStart) {
			possibleRootStoreIntovalue(processList1, aProcess);
		}
		longAtput(((((char *) processList1)) + 4) + (1 << 2), aProcess);
		/* begin storePointer:ofObject:withValue: */
		if (aProcess < youngStart) {
			possibleRootStoreIntovalue(aProcess, processList1);
		}
		longAtput(((((char *) aProcess)) + 4) + (3 << 2), processList1);
	}
}

int returnAtlastIndexlefttop(int stopIndex, int lastIndex, int left, int top) {
	stopCode = stObjectat(scanStopArray, stopIndex);
	if (!successFlag) {
		return null;
	}
	/* begin storeInteger:ofObject:withValue: */
	if ((lastIndex ^ (lastIndex << 1)) >= 0) {
		longAtput(((((char *) bitBltOop)) + 4) + (15 << 2), ((lastIndex << 1) | 1));
	} else {
		successFlag = 0;
	}
	if (scanDisplayFlag) {
		affectedL = left;
		affectedR = bbW + dx;
		affectedT = top;
		affectedB = bbH + dy;
	}
}

void reverseBytesFromto(int startAddr, int stopAddr) {
    int addr;

	addr = startAddr;
	while (addr < stopAddr) {
		longAtput(addr, ((((((unsigned) (longAt(addr)) >> 24)) & 255) + ((((unsigned) (longAt(addr)) >> 8)) & 65280)) + ((((unsigned) (longAt(addr)) << 8)) & 16711680)) + ((((unsigned) (longAt(addr)) << 24)) & 4278190080U));
		addr += 4;
	}
}

void reverseBytesInImage(void) {
    int startAddr;
    int addr;
    int methodHeader;
    int wordAddr;
    int oop;
    int fmt;
    int stopAddr;
    int addr1;
    int chunk;
    int extra;
    int type;
    int extra1;
    int sz;
    int header;
    int extra2;
    int type1;
    int extra11;

	/* begin reverseBytesFrom:to: */
	startAddr = startOfMemory();
	addr = startAddr;
	while (addr < endOfMemory) {
		longAtput(addr, ((((((unsigned) (longAt(addr)) >> 24)) & 255) + ((((unsigned) (longAt(addr)) >> 8)) & 65280)) + ((((unsigned) (longAt(addr)) << 8)) & 16711680)) + ((((unsigned) (longAt(addr)) << 24)) & 4278190080U));
		addr += 4;
	}
	/* begin byteSwapByteObjects */
	/* begin oopFromChunk: */
	chunk = startOfMemory();
	/* begin extraHeaderBytes: */
	type = (longAt(chunk)) & 3;
	if (type > 1) {
		extra1 = 0;
	} else {
		if (type == 1) {
			extra1 = 4;
		} else {
			extra1 = 8;
		}
	}
	extra = extra1;
	oop = chunk + extra;
	while (oop < endOfMemory) {
		if (!(((longAt(oop)) & 3) == 2)) {
			fmt = (((unsigned) (longAt(oop))) >> 8) & 15;
			if (fmt >= 8) {
				wordAddr = oop + 4;
				if (fmt >= 12) {
					methodHeader = longAt(oop + 4);
					wordAddr = (wordAddr + 4) + (((((unsigned) methodHeader) >> 10) & 255) * 4);
				}
				/* begin reverseBytesFrom:to: */
				stopAddr = oop + (sizeBitsOf(oop));
				addr1 = wordAddr;
				while (addr1 < stopAddr) {
					longAtput(addr1, ((((((unsigned) (longAt(addr1)) >> 24)) & 255) + ((((unsigned) (longAt(addr1)) >> 8)) & 65280)) + ((((unsigned) (longAt(addr1)) << 8)) & 16711680)) + ((((unsigned) (longAt(addr1)) << 24)) & 4278190080U));
					addr1 += 4;
				}
			}
		}
		/* begin objectAfter: */
		;
		if (((longAt(oop)) & 3) == 2) {
			sz = (longAt(oop)) & 4294967292U;
		} else {
			/* begin sizeBitsOf: */
			header = longAt(oop);
			if ((header & 3) == 0) {
				sz = (longAt(oop - 8)) & 4294967292U;
				goto l1;
			} else {
				sz = header & 252;
				goto l1;
			}
		l1:	/* end sizeBitsOf: */;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type1 = (longAt(oop + sz)) & 3;
		if (type1 > 1) {
			extra11 = 0;
		} else {
			if (type1 == 1) {
				extra11 = 4;
			} else {
				extra11 = 8;
			}
		}
		extra2 = extra11;
		oop = (oop + sz) + extra2;
	}
}

int rgbAddwith(int sourceWord, int destinationWord) {
	if (destPixSize < 16) {
		return partitionedAddtonBitsnPartitions(sourceWord, destinationWord, destPixSize, pixPerWord);
	}
	if (destPixSize == 16) {
		return (partitionedAddtonBitsnPartitions(sourceWord, destinationWord, 5, 3)) + ((partitionedAddtonBitsnPartitions(((unsigned) sourceWord) >> 16, ((unsigned) destinationWord) >> 16, 5, 3)) << 16);
	} else {
		return partitionedAddtonBitsnPartitions(sourceWord, destinationWord, 8, 3);
	}
}

int rgbDiffwith(int sourceWord, int destinationWord) {
    int destPixVal;
    int pixMask;
    int destShifted;
    int sourceShifted;
    int sourcePixVal;
    int diff;
    int maskShifted;
    int bitsPerColor;
    int i;
    int rgbMask;

	pixMask = maskTable[destPixSize];
	if (destPixSize == 16) {
		bitsPerColor = 5;
		rgbMask = 31;
	} else {
		bitsPerColor = 8;
		rgbMask = 255;
	}
	maskShifted = destMask;
	destShifted = destinationWord;
	sourceShifted = sourceWord;
	for (i = 1; i <= pixPerWord; i += 1) {
		if ((maskShifted & pixMask) > 0) {
			destPixVal = destShifted & pixMask;
			sourcePixVal = sourceShifted & pixMask;
			if (destPixSize < 16) {
				if (sourcePixVal == destPixVal) {
					diff = 0;
				} else {
					diff = 1;
				}
			} else {
				diff = partitionedSubfromnBitsnPartitions(sourcePixVal, destPixVal, bitsPerColor, 3);
				diff = ((diff & rgbMask) + ((((unsigned) diff) >> bitsPerColor) & rgbMask)) + ((((unsigned) (((unsigned) diff) >> bitsPerColor)) >> bitsPerColor) & rgbMask);
			}
			bitCount += diff;
		}
		maskShifted = ((unsigned) maskShifted) >> destPixSize;
		sourceShifted = ((unsigned) sourceShifted) >> destPixSize;
		destShifted = ((unsigned) destShifted) >> destPixSize;
	}
	return destinationWord;
}

int rgbMapfromto(int sourcePixel, int nBitsIn, int nBitsOut) {
    int mask;
    int srcPix;
    int destPix;
    int d;

	if ((d = nBitsOut - nBitsIn) > 0) {
		mask = (1 << nBitsIn) - 1;
		srcPix = sourcePixel << d;
		mask = mask << d;
		destPix = srcPix & mask;
		mask = mask << nBitsOut;
		srcPix = srcPix << d;
		return (destPix + (srcPix & mask)) + ((srcPix << d) & (mask << nBitsOut));
	} else {
		if (d == 0) {
			return sourcePixel;
		}
		if (sourcePixel == 0) {
			return sourcePixel;
		}
		d = nBitsIn - nBitsOut;
		mask = (1 << nBitsOut) - 1;
		srcPix = ((unsigned) sourcePixel) >> d;
		destPix = srcPix & mask;
		mask = mask << nBitsOut;
		srcPix = ((unsigned) srcPix) >> d;
		destPix = (destPix + (srcPix & mask)) + ((((unsigned) srcPix) >> d) & (mask << nBitsOut));
		if (destPix == 0) {
			return 1;
		}
		return destPix;
	}
}

int rgbMaxwith(int sourceWord, int destinationWord) {
    int mask;
    int i;
    int result;
    int mask3;
    int i1;
    int result1;

	if (destPixSize < 16) {
		/* begin partitionedMax:with:nBits:nPartitions: */
		mask = maskTable[destPixSize];
		result = 0;
		for (i = 1; i <= pixPerWord; i += 1) {
			result = result | ((((destinationWord & mask) < (sourceWord & mask)) ? (sourceWord & mask) : (destinationWord & mask)));
			mask = mask << destPixSize;
		}
		return result;
	}
	if (destPixSize == 16) {
		return (partitionedMaxwithnBitsnPartitions(sourceWord, destinationWord, 5, 3)) + ((partitionedMaxwithnBitsnPartitions(((unsigned) sourceWord) >> 16, ((unsigned) destinationWord) >> 16, 5, 3)) << 16);
	} else {
		/* begin partitionedMax:with:nBits:nPartitions: */
		mask3 = maskTable[8];
		result1 = 0;
		for (i1 = 1; i1 <= 3; i1 += 1) {
			result1 = result1 | ((((destinationWord & mask3) < (sourceWord & mask3)) ? (sourceWord & mask3) : (destinationWord & mask3)));
			mask3 = mask3 << 8;
		}
		return result1;
	}
}

int rgbMinwith(int sourceWord, int destinationWord) {
    int mask;
    int i;
    int result;
    int mask3;
    int i1;
    int result1;

	if (destPixSize < 16) {
		/* begin partitionedMin:with:nBits:nPartitions: */
		mask = maskTable[destPixSize];
		result = 0;
		for (i = 1; i <= pixPerWord; i += 1) {
			result = result | ((((destinationWord & mask) < (sourceWord & mask)) ? (destinationWord & mask) : (sourceWord & mask)));
			mask = mask << destPixSize;
		}
		return result;
	}
	if (destPixSize == 16) {
		return (partitionedMinwithnBitsnPartitions(sourceWord, destinationWord, 5, 3)) + ((partitionedMinwithnBitsnPartitions(((unsigned) sourceWord) >> 16, ((unsigned) destinationWord) >> 16, 5, 3)) << 16);
	} else {
		/* begin partitionedMin:with:nBits:nPartitions: */
		mask3 = maskTable[8];
		result1 = 0;
		for (i1 = 1; i1 <= 3; i1 += 1) {
			result1 = result1 | ((((destinationWord & mask3) < (sourceWord & mask3)) ? (destinationWord & mask3) : (sourceWord & mask3)));
			mask3 = mask3 << 8;
		}
		return result1;
	}
}

int rgbMinInvertwith(int wordToInvert, int destinationWord) {
    int sourceWord;
    int mask;
    int i;
    int result;
    int mask3;
    int i1;
    int result1;

	sourceWord = ~wordToInvert;
	if (destPixSize < 16) {
		/* begin partitionedMin:with:nBits:nPartitions: */
		mask = maskTable[destPixSize];
		result = 0;
		for (i = 1; i <= pixPerWord; i += 1) {
			result = result | ((((destinationWord & mask) < (sourceWord & mask)) ? (destinationWord & mask) : (sourceWord & mask)));
			mask = mask << destPixSize;
		}
		return result;
	}
	if (destPixSize == 16) {
		return (partitionedMinwithnBitsnPartitions(sourceWord, destinationWord, 5, 3)) + ((partitionedMinwithnBitsnPartitions(((unsigned) sourceWord) >> 16, ((unsigned) destinationWord) >> 16, 5, 3)) << 16);
	} else {
		/* begin partitionedMin:with:nBits:nPartitions: */
		mask3 = maskTable[8];
		result1 = 0;
		for (i1 = 1; i1 <= 3; i1 += 1) {
			result1 = result1 | ((((destinationWord & mask3) < (sourceWord & mask3)) ? (destinationWord & mask3) : (sourceWord & mask3)));
			mask3 = mask3 << 8;
		}
		return result1;
	}
}

int rgbSubwith(int sourceWord, int destinationWord) {
	if (destPixSize < 16) {
		return partitionedSubfromnBitsnPartitions(sourceWord, destinationWord, destPixSize, pixPerWord);
	}
	if (destPixSize == 16) {
		return (partitionedSubfromnBitsnPartitions(sourceWord, destinationWord, 5, 3)) + ((partitionedSubfromnBitsnPartitions(((unsigned) sourceWord) >> 16, ((unsigned) destinationWord) >> 16, 5, 3)) << 16);
	} else {
		return partitionedSubfromnBitsnPartitions(sourceWord, destinationWord, 8, 3);
	}
}

int rightType(int headerWord) {
	if ((headerWord & 252) == 0) {
		return 0;
	} else {
		if ((headerWord & 126976) == 0) {
			return 1;
		} else {
			return 3;
		}
	}
}

int scanCharacters(void) {
    int sourceX2;
    int ascii;
    int top;
    int nextDestX;
    int charVal;
    int left;
    int lastIndex;
    int lastIndex1;

	if (scanDisplayFlag) {
		clipRange();
		left = dx;
		top = dy;
	}
	lastIndex = scanStart;
	while (lastIndex <= scanStop) {
		charVal = stObjectat(scanString, lastIndex);
		ascii = (charVal >> 1);
		if (!successFlag) {
			return null;
		}
		stopCode = stObjectat(scanStopArray, ascii + 1);
		if (!successFlag) {
			return null;
		}
		if (!(stopCode == nilObj)) {
			/* begin returnAt:lastIndex:left:top: */
			stopCode = stObjectat(scanStopArray, ascii + 1);
			if (!successFlag) {
				return null;
			}
			/* begin storeInteger:ofObject:withValue: */
			if ((lastIndex ^ (lastIndex << 1)) >= 0) {
				longAtput(((((char *) bitBltOop)) + 4) + (15 << 2), ((lastIndex << 1) | 1));
			} else {
				successFlag = 0;
			}
			if (scanDisplayFlag) {
				affectedL = left;
				affectedR = bbW + dx;
				affectedT = top;
				affectedB = bbH + dy;
			}
			return null;
		}
		sourceX = stObjectat(scanXTable, ascii + 1);
		sourceX2 = stObjectat(scanXTable, ascii + 2);
		if (!successFlag) {
			return null;
		}
		if (((sourceX & 1)) && ((sourceX2 & 1))) {
			sourceX = (sourceX >> 1);
			sourceX2 = (sourceX2 >> 1);
		} else {
			successFlag = 0;
			return null;
		}
		nextDestX = destX + (width = sourceX2 - sourceX);
		if (nextDestX > scanRightX) {
			/* begin returnAt:lastIndex:left:top: */
			stopCode = stObjectat(scanStopArray, 258);
			if (!successFlag) {
				return null;
			}
			/* begin storeInteger:ofObject:withValue: */
			if ((lastIndex ^ (lastIndex << 1)) >= 0) {
				longAtput(((((char *) bitBltOop)) + 4) + (15 << 2), ((lastIndex << 1) | 1));
			} else {
				successFlag = 0;
			}
			if (scanDisplayFlag) {
				affectedL = left;
				affectedR = bbW + dx;
				affectedT = top;
				affectedB = bbH + dy;
			}
			return null;
		}
		if (scanDisplayFlag) {
			copyBits();
		}
		destX = nextDestX;
		/* begin storeInteger:ofObject:withValue: */
		if ((destX ^ (destX << 1)) >= 0) {
			longAtput(((((char *) bitBltOop)) + 4) + (4 << 2), ((destX << 1) | 1));
		} else {
			successFlag = 0;
		}
		lastIndex += 1;
	}
	/* begin returnAt:lastIndex:left:top: */
	lastIndex1 = scanStop;
	stopCode = stObjectat(scanStopArray, 257);
	if (!successFlag) {
		goto l1;
	}
	/* begin storeInteger:ofObject:withValue: */
	if ((lastIndex1 ^ (lastIndex1 << 1)) >= 0) {
		longAtput(((((char *) bitBltOop)) + 4) + (15 << 2), ((lastIndex1 << 1) | 1));
	} else {
		successFlag = 0;
	}
	if (scanDisplayFlag) {
		affectedL = left;
		affectedR = bbW + dx;
		affectedT = top;
		affectedB = bbH + dy;
	}
l1:	/* end returnAt:lastIndex:left:top: */;
}

int schedulerPointer(void) {
	return longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2));
}

void setSizeOfFreeto(int chunk, int byteSize) {
	longAtput(chunk, (byteSize & 4294967292U) | 2);
}

int showDisplayBits(void) {
    int displayObj;
    int dispBits;
    int affectedRectL;
    int affectedRectR;
    int affectedRectT;
    int affectedRectB;
    int dispBitsIndex;
    int h;
    int w;
    int d;
    int successValue;

	if (deferDisplayUpdates) {
		return null;
	}
	displayObj = longAt(((((char *) specialObjectsOop)) + 4) + (14 << 2));
	if (!(destForm == displayObj)) {
		return null;
	}
	/* begin success: */
	successValue = (((((unsigned) (longAt(displayObj))) >> 8) & 15) <= 4) && ((lengthOf(displayObj)) >= 4);
	successFlag = successValue && successFlag;
	if (successFlag) {
		dispBits = longAt(((((char *) displayObj)) + 4) + (0 << 2));
		w = fetchIntegerofObject(1, displayObj);
		h = fetchIntegerofObject(2, displayObj);
		d = fetchIntegerofObject(3, displayObj);
	}
	if (successFlag) {
		affectedRectL = affectedL;
		affectedRectR = affectedR;
		affectedRectT = affectedT;
		affectedRectB = affectedB;
		dispBitsIndex = dispBits + 4;
		ioShowDisplay(dispBitsIndex, w, h, d, affectedRectL, affectedRectR, affectedRectT, affectedRectB);
	}
}

int signExtend16(int int16) {
	if ((int16 & 32768) == 0) {
		return int16;
	} else {
		return int16 - 65536;
	}
}

void signalFinalization(int weakReferenceOop) {
	interruptCheckCounter = 0;
	pendingFinalizationSignals += 1;
}

int signalSemaphoreWithIndex(int index) {
    int i;

	if (index <= 0) {
		return null;
	}
	interruptCheckCounter = 0;
	for (i = 1; i <= semaphoresToSignalCount; i += 1) {
		if ((semaphoresToSignal[i]) == index) {
			return null;
		}
	}
	if (semaphoresToSignalCount < 5) {
		semaphoresToSignalCount += 1;
		semaphoresToSignal[semaphoresToSignalCount] = index;
	}
}

int sizeBitsOf(int oop) {
    int header;

	header = longAt(oop);
	if ((header & 3) == 0) {
		return (longAt(oop - 8)) & 4294967292U;
	} else {
		return header & 252;
	}
}

int sizeBitsOfSafe(int oop) {
    int header;
    int type;

	header = longAt(oop);
	/* begin rightType: */
	if ((header & 252) == 0) {
		type = 0;
		goto l1;
	} else {
		if ((header & 126976) == 0) {
			type = 1;
			goto l1;
		} else {
			type = 3;
			goto l1;
		}
	}
l1:	/* end rightType: */;
	if (type == 0) {
		return (longAt(oop - 8)) & 4294967292U;
	} else {
		return header & 252;
	}
}

int sizeHeader(int oop) {
	return longAt(oop - 8);
}

int sizeOfFree(int oop) {
	return (longAt(oop)) & 4294967292U;
}

int sizeOfSTArrayFromCPrimitive(void *cPtr) {
    int oop;
    int header;
    int sz;

	oop = ((int) cPtr) - 4;
	if (!(isWordsOrBytes(oop))) {
		successFlag = 0;
		return 0;
	}
	/* begin lengthOf: */
	header = longAt(oop);
	/* begin lengthOf:baseHeader:format: */
	if ((header & 3) == 0) {
		sz = (longAt(oop - 8)) & 4294967292U;
	} else {
		sz = header & 252;
	}
	if (((((unsigned) header) >> 8) & 15) < 8) {
		return ((unsigned) (sz - 4)) >> 2;
	} else {
		return (sz - 4) - (((((unsigned) header) >> 8) & 15) & 3);
	}
	return null;
}

int smoothPixatXfyfdxhdyhdxvdyvpixPerWordpixelMasksourceMap(int n, int xf, int yf, int dxh, int dyh, int dxv, int dyv, int srcPixPerWord, int sourcePixMask, int sourceMap) {
    int j;
    int sourcePix;
    int b;
    int x;
    int y;
    int bitsPerColor;
    int nPix;
    int maxPix;
    int i;
    int d;
    int rgb;
    int g;
    int r;
    int mask;
    int srcPix;
    int destPix;
    int d1;

	r = g = b = 0;
	maxPix = n * n;
	x = xf;
	y = yf;
	nPix = 0;
	for (i = 0; i <= (n - 1); i += 1) {
		for (j = 0; j <= (n - 1); j += 1) {
			sourcePix = (sourcePixAtXypixPerWord(((unsigned) ((x + (dxh * i)) + (dxv * j))) >> 14, ((unsigned) ((y + (dyh * i)) + (dyv * j))) >> 14, srcPixPerWord)) & sourcePixMask;
			if (!((combinationRule == 25) && (sourcePix == 0))) {
				nPix += 1;
				if (sourcePixSize < 16) {
					rgb = (longAt(((((char *) sourceMap)) + 4) + (sourcePix << 2))) & 16777215;
				} else {
					if (sourcePixSize == 32) {
						rgb = sourcePix & 16777215;
					} else {
						/* begin rgbMap:from:to: */
						if ((d1 = 8 - 5) > 0) {
							mask = (1 << 5) - 1;
							srcPix = sourcePix << d1;
							mask = mask << d1;
							destPix = srcPix & mask;
							mask = mask << 8;
							srcPix = srcPix << d1;
							rgb = (destPix + (srcPix & mask)) + ((srcPix << d1) & (mask << 8));
							goto l1;
						} else {
							if (d1 == 0) {
								rgb = sourcePix;
								goto l1;
							}
							if (sourcePix == 0) {
								rgb = sourcePix;
								goto l1;
							}
							d1 = 5 - 8;
							mask = (1 << 8) - 1;
							srcPix = ((unsigned) sourcePix) >> d1;
							destPix = srcPix & mask;
							mask = mask << 8;
							srcPix = ((unsigned) srcPix) >> d1;
							destPix = (destPix + (srcPix & mask)) + ((((unsigned) srcPix) >> d1) & (mask << 8));
							if (destPix == 0) {
								rgb = 1;
								goto l1;
							}
							rgb = destPix;
							goto l1;
						}
					l1:	/* end rgbMap:from:to: */;
					}
				}
				r += (((unsigned) rgb) >> 16) & 255;
				g += (((unsigned) rgb) >> 8) & 255;
				b += rgb & 255;
			}
		}
	}
	if ((nPix == 0) || ((combinationRule == 25) && (nPix < (((int) maxPix >> 1))))) {
		return 0;
	}
	if (colorMap != nilObj) {
		bitsPerColor = cmBitsPerColor;
	} else {
		if (destPixSize == 16) {
			bitsPerColor = 5;
		}
		if (destPixSize == 32) {
			bitsPerColor = 8;
		}
	}
	d = 8 - bitsPerColor;
	rgb = (((((unsigned) (r / nPix)) >> d) << (bitsPerColor * 2)) + ((((unsigned) (g / nPix)) >> d) << bitsPerColor)) + (((unsigned) (b / nPix)) >> d);
	if (rgb == 0) {
		if (((r + g) + b) > 0) {
			rgb = 1;
		}
	}
	if (colorMap != nilObj) {
		return longAt(((((char *) colorMap)) + 4) + (rgb << 2));
	} else {
		return rgb;
	}
}

int sourcePixAtXypixPerWord(int x, int y, int srcPixPerWord) {
    int sourceWord;
    int index;

	if ((x < 0) || (x >= srcWidth)) {
		return 0;
	}
	if ((y < 0) || (y >= srcHeight)) {
		return 0;
	}
	index = ((y * sourceRaster) + (x / srcPixPerWord)) * 4;
	sourceWord = longAt((sourceBits + 4) + index);
	return ((unsigned) sourceWord) >> ((32 - sourcePixSize) - ((x % srcPixPerWord) * sourcePixSize));
}

void sourceSkewAndPointerInit(void) {
    int dWid;
    int sxLowBits;
    int dxLowBits;
    int pixPerM1;

	pixPerM1 = pixPerWord - 1;
	sxLowBits = sx & pixPerM1;
	dxLowBits = dx & pixPerM1;
	if (hDir > 0) {
		dWid = ((bbW < (pixPerWord - dxLowBits)) ? bbW : (pixPerWord - dxLowBits));
		preload = (sxLowBits + dWid) > pixPerM1;
	} else {
		dWid = ((bbW < (dxLowBits + 1)) ? bbW : (dxLowBits + 1));
		preload = ((sxLowBits - dWid) + 1) < 0;
	}
	skew = (sxLowBits - dxLowBits) * destPixSize;
	if (preload) {
		if (skew < 0) {
			skew += 32;
		} else {
			skew -= 32;
		}
	}
	sourceIndex = (sourceBits + 4) + (((sy * sourceRaster) + (sx / (32 / sourcePixSize))) * 4);
	sourceDelta = 4 * ((sourceRaster * vDir) - (nWords * hDir));
	if (preload) {
		sourceDelta -= 4 * hDir;
	}
}

int sourceWordwith(int sourceWord, int destinationWord) {
	return sourceWord;
}

int specialSelector(int index) {
	return longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (23 << 2))))) + 4) + ((index * 2) << 2));
}

int splObj(int index) {
	return longAt(((((char *) specialObjectsOop)) + 4) + (index << 2));
}

int stObjectat(int array, int index) {
    int stSize;
    int hdr;
    int totalLength;
    int fmt;
    int fixedFields;
    int sp;
    int sz;
    int classFormat;
    int cls;
    int ccIndex;

	hdr = longAt(array);
	fmt = (((unsigned) hdr) >> 8) & 15;
	/* begin lengthOf:baseHeader:format: */
	if ((hdr & 3) == 0) {
		sz = (longAt(array - 8)) & 4294967292U;
	} else {
		sz = hdr & 252;
	}
	if (fmt < 8) {
		totalLength = ((unsigned) (sz - 4)) >> 2;
		goto l2;
	} else {
		totalLength = (sz - 4) - (fmt & 3);
		goto l2;
	}
l2:	/* end lengthOf:baseHeader:format: */;
	/* begin fixedFieldsOf:format:length: */
	if ((fmt > 4) || (fmt == 2)) {
		fixedFields = 0;
		goto l3;
	}
	if (fmt < 2) {
		fixedFields = totalLength;
		goto l3;
	}
	/* begin fetchClassOf: */
	if ((array & 1)) {
		cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l4;
	}
	ccIndex = (((unsigned) (longAt(array))) >> 12) & 31;
	if (ccIndex == 0) {
		cls = (longAt(array - 4)) & 4294967292U;
		goto l4;
	} else {
		cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l4;
	}
l4:	/* end fetchClassOf: */;
	classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
	fixedFields = (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
l3:	/* end fixedFieldsOf:format:length: */;
	if ((fmt == 3) && (isContextHeader(hdr))) {
		/* begin fetchStackPointerOf: */
		sp = longAt(((((char *) array)) + 4) + (2 << 2));
		if (!((sp & 1))) {
			stSize = 0;
			goto l1;
		}
		stSize = (sp >> 1);
	l1:	/* end fetchStackPointerOf: */;
	} else {
		stSize = totalLength - fixedFields;
	}
	if (((((unsigned ) index)) >= 1) && ((((unsigned ) index)) <= (((unsigned ) stSize)))) {
		/* begin subscript:with:format: */
		if (fmt <= 4) {
			return longAt(((((char *) array)) + 4) + (((index + fixedFields) - 1) << 2));
		}
		if (fmt < 8) {
			return positive32BitIntegerFor(longAt(((((char *) array)) + 4) + (((index + fixedFields) - 1) << 2)));
		} else {
			return (((byteAt(((((char *) array)) + 4) + ((index + fixedFields) - 1))) << 1) | 1);
		}
		return null;
	} else {
		successFlag = 0;
		return 0;
	}
}

void stObjectatput(int array, int index, int value) {
    int stSize;
    int hdr;
    int totalLength;
    int fmt;
    int fixedFields;
    int sp;
    int sz;
    int valueToStore;
    int classFormat;
    int cls;
    int ccIndex;

	hdr = longAt(array);
	fmt = (((unsigned) hdr) >> 8) & 15;
	/* begin lengthOf:baseHeader:format: */
	if ((hdr & 3) == 0) {
		sz = (longAt(array - 8)) & 4294967292U;
	} else {
		sz = hdr & 252;
	}
	if (fmt < 8) {
		totalLength = ((unsigned) (sz - 4)) >> 2;
		goto l2;
	} else {
		totalLength = (sz - 4) - (fmt & 3);
		goto l2;
	}
l2:	/* end lengthOf:baseHeader:format: */;
	/* begin fixedFieldsOf:format:length: */
	if ((fmt > 4) || (fmt == 2)) {
		fixedFields = 0;
		goto l3;
	}
	if (fmt < 2) {
		fixedFields = totalLength;
		goto l3;
	}
	/* begin fetchClassOf: */
	if ((array & 1)) {
		cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l4;
	}
	ccIndex = (((unsigned) (longAt(array))) >> 12) & 31;
	if (ccIndex == 0) {
		cls = (longAt(array - 4)) & 4294967292U;
		goto l4;
	} else {
		cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l4;
	}
l4:	/* end fetchClassOf: */;
	classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
	fixedFields = (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
l3:	/* end fixedFieldsOf:format:length: */;
	if ((fmt == 3) && (isContextHeader(hdr))) {
		/* begin fetchStackPointerOf: */
		sp = longAt(((((char *) array)) + 4) + (2 << 2));
		if (!((sp & 1))) {
			stSize = 0;
			goto l1;
		}
		stSize = (sp >> 1);
	l1:	/* end fetchStackPointerOf: */;
	} else {
		stSize = totalLength - fixedFields;
	}
	if (((((unsigned ) index)) >= 1) && ((((unsigned ) index)) <= (((unsigned ) stSize)))) {
		/* begin subscript:with:storing:format: */
		if (fmt <= 4) {
			/* begin storePointer:ofObject:withValue: */
			if (array < youngStart) {
				possibleRootStoreIntovalue(array, value);
			}
			longAtput(((((char *) array)) + 4) + (((index + fixedFields) - 1) << 2), value);
		} else {
			if (fmt < 8) {
				valueToStore = positive32BitValueOf(value);
				if (successFlag) {
					longAtput(((((char *) array)) + 4) + (((index + fixedFields) - 1) << 2), valueToStore);
				}
			} else {
				if (!((value & 1))) {
					successFlag = 0;
				}
				valueToStore = (value >> 1);
				if (!((valueToStore >= 0) && (valueToStore <= 255))) {
					successFlag = 0;
				}
				if (successFlag) {
					byteAtput(((((char *) array)) + 4) + ((index + fixedFields) - 1), valueToStore);
				}
			}
		}
	} else {
		successFlag = 0;
	}
}

int stSizeOf(int oop) {
    int hdr;
    int totalLength;
    int fmt;
    int fixedFields;
    int sp;
    int sz;
    int classFormat;
    int cls;
    int ccIndex;

	hdr = longAt(oop);
	fmt = (((unsigned) hdr) >> 8) & 15;
	/* begin lengthOf:baseHeader:format: */
	if ((hdr & 3) == 0) {
		sz = (longAt(oop - 8)) & 4294967292U;
	} else {
		sz = hdr & 252;
	}
	if (fmt < 8) {
		totalLength = ((unsigned) (sz - 4)) >> 2;
		goto l1;
	} else {
		totalLength = (sz - 4) - (fmt & 3);
		goto l1;
	}
l1:	/* end lengthOf:baseHeader:format: */;
	/* begin fixedFieldsOf:format:length: */
	if ((fmt > 4) || (fmt == 2)) {
		fixedFields = 0;
		goto l2;
	}
	if (fmt < 2) {
		fixedFields = totalLength;
		goto l2;
	}
	/* begin fetchClassOf: */
	if ((oop & 1)) {
		cls = longAt(((((char *) specialObjectsOop)) + 4) + (5 << 2));
		goto l3;
	}
	ccIndex = (((unsigned) (longAt(oop))) >> 12) & 31;
	if (ccIndex == 0) {
		cls = (longAt(oop - 4)) & 4294967292U;
		goto l3;
	} else {
		cls = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (28 << 2))))) + 4) + ((ccIndex - 1) << 2));
		goto l3;
	}
l3:	/* end fetchClassOf: */;
	classFormat = (longAt(((((char *) cls)) + 4) + (2 << 2))) - 1;
	fixedFields = (((((unsigned) classFormat) >> 11) & 192) + ((((unsigned) classFormat) >> 2) & 63)) - 1;
l2:	/* end fixedFieldsOf:format:length: */;
	if ((fmt == 3) && (isContextHeader(hdr))) {
		/* begin fetchStackPointerOf: */
		sp = longAt(((((char *) oop)) + 4) + (2 << 2));
		if (!((sp & 1))) {
			return 0;
		}
		return (sp >> 1);
	} else {
		return totalLength - fixedFields;
	}
}

int stackIntegerValue(int offset) {
    int integerPointer;

	integerPointer = longAt(stackPointer - (offset * 4));
	if ((integerPointer & 1)) {
		return (integerPointer >> 1);
	} else {
		successFlag = 0;
		return 0;
	}
}

int stackPointerIndex(void) {
	return ((unsigned) ((stackPointer - activeContext) - 4)) >> 2;
}

int stackTop(void) {
	return longAt(stackPointer);
}

int stackValue(int offset) {
	return longAt(stackPointer - (offset * 4));
}

int startField(void) {
    int typeBits;
    int childType;

	child = longAt(field);
	typeBits = child & 3;
	if ((typeBits & 1) == 1) {
		field -= 4;
		return 1;
	}
	if (typeBits == 0) {
		longAtput(field, parentField);
		parentField = field;
		return 2;
	}
	if (typeBits == 2) {
		if ((child & 126976) != 0) {
			child = child & 4294967292U;
			/* begin rightType: */
			if ((child & 252) == 0) {
				childType = 0;
				goto l1;
			} else {
				if ((child & 126976) == 0) {
					childType = 1;
					goto l1;
				} else {
					childType = 3;
					goto l1;
				}
			}
		l1:	/* end rightType: */;
			longAtput(field, child | childType);
			return 3;
		} else {
			child = longAt(field - 4);
			child = child & 4294967292U;
			longAtput(field - 4, parentField);
			parentField = (field - 4) | 1;
			return 2;
		}
	}
}

int startObj(void) {
    int oop;
    int lastFieldOffset;
    int header;
    int methodHeader;
    int sz;
    int fmt;
    int header1;
    int header2;
    int type;

	oop = child;
	if (oop < youngStart) {
		field = oop;
		return 3;
	}
	header = longAt(oop);
	if ((header & 2147483648U) == 0) {
		if (((((unsigned) (longAt(oop))) >> 8) & 15) == 4) {
			lastFieldOffset = (nonWeakFieldsOf(oop)) << 2;
		} else {
			/* begin lastPointerOf: */
			header1 = longAt(oop);
			fmt = (((unsigned) header1) >> 8) & 15;
			if (fmt <= 4) {
				if ((fmt == 3) && (isContextHeader(header1))) {
					lastFieldOffset = (6 + (fetchStackPointerOf(oop))) * 4;
					goto l1;
				}
				/* begin sizeBitsOfSafe: */
				header2 = longAt(oop);
				/* begin rightType: */
				if ((header2 & 252) == 0) {
					type = 0;
					goto l2;
				} else {
					if ((header2 & 126976) == 0) {
						type = 1;
						goto l2;
					} else {
						type = 3;
						goto l2;
					}
				}
			l2:	/* end rightType: */;
				if (type == 0) {
					sz = (longAt(oop - 8)) & 4294967292U;
					goto l3;
				} else {
					sz = header2 & 252;
					goto l3;
				}
			l3:	/* end sizeBitsOfSafe: */;
				lastFieldOffset = sz - 4;
				goto l1;
			}
			if (fmt < 12) {
				lastFieldOffset = 0;
				goto l1;
			}
			methodHeader = longAt(oop + 4);
			lastFieldOffset = (((((unsigned) methodHeader) >> 10) & 255) * 4) + 4;
		l1:	/* end lastPointerOf: */;
		}
		header = header & 4294967292U;
		header = (header | 2147483648U) | 2;
		longAtput(oop, header);
		field = oop + lastFieldOffset;
		return 1;
	} else {
		field = oop;
		return 3;
	}
}

int startOfMemory(void) {
	return (int) memory;
}

int stopReason(void) {
	return stopCode;
}

int storeByteofObjectwithValue(int byteIndex, int oop, int valueByte) {
	return byteAtput(((((char *) oop)) + 4) + byteIndex, valueByte);
}

void storeContextRegisters(int activeCntx) {
	longAtput(((((char *) activeCntx)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
	longAtput(((((char *) activeCntx)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
}

void storeIntegerofObjectwithValue(int fieldIndex, int objectPointer, int integerValue) {
	if ((integerValue ^ (integerValue << 1)) >= 0) {
		longAtput(((((char *) objectPointer)) + 4) + (fieldIndex << 2), ((integerValue << 1) | 1));
	} else {
		successFlag = 0;
	}
}

int storePointerofObjectwithValue(int fieldIndex, int oop, int valuePointer) {
	if (oop < youngStart) {
		possibleRootStoreIntovalue(oop, valuePointer);
	}
	return longAtput(((((char *) oop)) + 4) + (fieldIndex << 2), valuePointer);
}

int storePointerUncheckedofObjectwithValue(int fieldIndex, int oop, int valuePointer) {
	return longAtput(((((char *) oop)) + 4) + (fieldIndex << 2), valuePointer);
}

void storeStackPointerValueinContext(int value, int contextPointer) {
	longAtput(((((char *) contextPointer)) + 4) + (2 << 2), ((value << 1) | 1));
}

int storeWordofObjectwithValue(int fieldIndex, int oop, int valueWord) {
	return longAtput(((((char *) oop)) + 4) + (fieldIndex << 2), valueWord);
}

int subWordwith(int sourceWord, int destinationWord) {
	return sourceWord - destinationWord;
}

int subscriptwithformat(int array, int index, int fmt) {
	if (fmt <= 4) {
		return longAt(((((char *) array)) + 4) + ((index - 1) << 2));
	}
	if (fmt < 8) {
		return positive32BitIntegerFor(longAt(((((char *) array)) + 4) + ((index - 1) << 2)));
	} else {
		return (((byteAt(((((char *) array)) + 4) + (index - 1))) << 1) | 1);
	}
}

void subscriptwithstoringformat(int array, int index, int oopToStore, int fmt) {
    int valueToStore;

	if (fmt <= 4) {
		/* begin storePointer:ofObject:withValue: */
		if (array < youngStart) {
			possibleRootStoreIntovalue(array, oopToStore);
		}
		longAtput(((((char *) array)) + 4) + ((index - 1) << 2), oopToStore);
	} else {
		if (fmt < 8) {
			valueToStore = positive32BitValueOf(oopToStore);
			if (successFlag) {
				longAtput(((((char *) array)) + 4) + ((index - 1) << 2), valueToStore);
			}
		} else {
			if (!((oopToStore & 1))) {
				successFlag = 0;
			}
			valueToStore = (oopToStore >> 1);
			if (!((valueToStore >= 0) && (valueToStore <= 255))) {
				successFlag = 0;
			}
			if (successFlag) {
				byteAtput(((((char *) array)) + 4) + (index - 1), valueToStore);
			}
		}
	}
}

int success(int successValue) {
	return successFlag = successValue && successFlag;
}

int sufficientSpaceAfterGC(int minFree) {
	incrementalGC();
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) < (((unsigned ) minFree))) {
		if (signalLowSpace) {
			return 0;
		}
		fullGC();
		if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) < ((((unsigned ) minFree)) + 15000)) {
			return 0;
		}
	}
	return 1;
}

int sufficientSpaceToAllocate(int bytes) {
    int minFree;

	minFree = (lowSpaceThreshold + bytes) + 4;
	if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
		return 1;
	} else {
		return sufficientSpaceAfterGC(minFree);
	}
}

int sufficientSpaceToInstantiateindexableSize(int classOop, int size) {
    int okay;
    int format;
    int minFree;
    int minFree1;

	format = (((unsigned) ((longAt(((((char *) classOop)) + 4) + (2 << 2))) - 1)) >> 8) & 15;
	if (((((unsigned ) size)) > 0) && (format < 2)) {
		return 0;
	}
	if (format < 8) {
		/* begin sufficientSpaceToAllocate: */
		minFree = (lowSpaceThreshold + (2500 + (size * 4))) + 4;
		if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree))) {
			okay = 1;
			goto l1;
		} else {
			okay = sufficientSpaceAfterGC(minFree);
			goto l1;
		}
	l1:	/* end sufficientSpaceToAllocate: */;
	} else {
		/* begin sufficientSpaceToAllocate: */
		minFree1 = (lowSpaceThreshold + (2500 + size)) + 4;
		if ((((unsigned ) ((longAt(freeBlock)) & 4294967292U))) >= (((unsigned ) minFree1))) {
			okay = 1;
			goto l2;
		} else {
			okay = sufficientSpaceAfterGC(minFree1);
			goto l2;
		}
	l2:	/* end sufficientSpaceToAllocate: */;
	}
	return okay;
}

int superclassOf(int classPointer) {
	return longAt(((((char *) classPointer)) + 4) + (0 << 2));
}

int sweepPhase(void) {
    int entriesAvailable;
    int survivors;
    int firstFree;
    int oopHeader;
    int oop;
    int freeChunk;
    int oopHeaderType;
    int hdrBytes;
    int oopSize;
    int freeChunkSize;
    int extra;
    int type;
    int extra1;
    int extra2;
    int type1;
    int extra11;

	entriesAvailable = fwdTableInit(8);
	survivors = 0;
	freeChunk = null;
	firstFree = null;
	/* begin oopFromChunk: */
	/* begin extraHeaderBytes: */
	type1 = (longAt(youngStart)) & 3;
	if (type1 > 1) {
		extra11 = 0;
	} else {
		if (type1 == 1) {
			extra11 = 4;
		} else {
			extra11 = 8;
		}
	}
	extra2 = extra11;
	oop = youngStart + extra2;
	while (oop < endOfMemory) {
		oopHeader = longAt(oop);
		oopHeaderType = oopHeader & 3;
		if (oopHeaderType == 3) {
			oopSize = oopHeader & 252;
			hdrBytes = 0;
		} else {
			if (oopHeaderType == 1) {
				oopSize = oopHeader & 252;
				hdrBytes = 4;
			} else {
				if (oopHeaderType == 0) {
					oopSize = (longAt(oop - 8)) & 4294967292U;
					hdrBytes = 8;
				} else {
					oopSize = oopHeader & 4294967292U;
					hdrBytes = 0;
				}
			}
		}
		if ((oopHeader & 2147483648U) == 0) {
			longAtput(oop - hdrBytes, 2);
			if (freeChunk != null) {
				freeChunkSize = (freeChunkSize + oopSize) + hdrBytes;
			} else {
				freeChunk = oop - hdrBytes;
				freeChunkSize = oopSize + (oop - freeChunk);
				if (firstFree == null) {
					firstFree = freeChunk;
				}
			}
		} else {
			longAtput(oop, oopHeader & 2147483647U);
			if (((((unsigned) (longAt(oop))) >> 8) & 15) == 4) {
				finalizeReference(oop);
			}
			if (entriesAvailable > 0) {
				entriesAvailable -= 1;
			} else {
				firstFree = freeChunk;
			}
			if (freeChunk != null) {
				longAtput(freeChunk, (freeChunkSize & 4294967292U) | 2);
			}
			freeChunk = null;
			survivors += 1;
		}
		/* begin oopFromChunk: */
		/* begin extraHeaderBytes: */
		type = (longAt(oop + oopSize)) & 3;
		if (type > 1) {
			extra1 = 0;
		} else {
			if (type == 1) {
				extra1 = 4;
			} else {
				extra1 = 8;
			}
		}
		extra = extra1;
		oop = (oop + oopSize) + extra;
	}
	if (freeChunk != null) {
		longAtput(freeChunk, (freeChunkSize & 4294967292U) | 2);
	}
	if (!(oop == endOfMemory)) {
		error("sweep failed to find exact end of memory");
	}
	if (firstFree == null) {
		error("expected to find at least one free object");
	} else {
		compStart = firstFree;
	}
	if (!(displayBits == 0)) {
		oopHeader = longAt(displayBits);
		longAtput(displayBits, oopHeader & 2147483647U);
	}
	return survivors;
}

void synchronousSignal(int aSemaphore) {
    int excessSignals;

	if ((longAt(((((char *) aSemaphore)) + 4) + (0 << 2))) == nilObj) {
		excessSignals = fetchIntegerofObject(2, aSemaphore);
		/* begin storeInteger:ofObject:withValue: */
		if (((excessSignals + 1) ^ ((excessSignals + 1) << 1)) >= 0) {
			longAtput(((((char *) aSemaphore)) + 4) + (2 << 2), (((excessSignals + 1) << 1) | 1));
		} else {
			successFlag = 0;
		}
	} else {
		resume(removeFirstLinkOfList(aSemaphore));
	}
}

int tallyIntoMapwith(int sourceWord, int destinationWord) {
    int pixVal;
    int mapIndex;
    int pixMask;
    int destShifted;
    int maskShifted;
    int i;
    int mask;
    int srcPix;
    int destPix;
    int d;
    int mask3;
    int srcPix1;
    int destPix1;
    int d1;

	if (colorMap == nilObj) {
		return destinationWord;
	}
	pixMask = maskTable[destPixSize];
	destShifted = destinationWord;
	maskShifted = destMask;
	for (i = 1; i <= pixPerWord; i += 1) {
		if (!((maskShifted & pixMask) == 0)) {
			pixVal = destShifted & pixMask;
			if (destPixSize < 16) {
				mapIndex = pixVal;
			} else {
				if (destPixSize == 16) {
					/* begin rgbMap:from:to: */
					if ((d = cmBitsPerColor - 5) > 0) {
						mask = (1 << 5) - 1;
						srcPix = pixVal << d;
						mask = mask << d;
						destPix = srcPix & mask;
						mask = mask << cmBitsPerColor;
						srcPix = srcPix << d;
						mapIndex = (destPix + (srcPix & mask)) + ((srcPix << d) & (mask << cmBitsPerColor));
						goto l1;
					} else {
						if (d == 0) {
							mapIndex = pixVal;
							goto l1;
						}
						if (pixVal == 0) {
							mapIndex = pixVal;
							goto l1;
						}
						d = 5 - cmBitsPerColor;
						mask = (1 << cmBitsPerColor) - 1;
						srcPix = ((unsigned) pixVal) >> d;
						destPix = srcPix & mask;
						mask = mask << cmBitsPerColor;
						srcPix = ((unsigned) srcPix) >> d;
						destPix = (destPix + (srcPix & mask)) + ((((unsigned) srcPix) >> d) & (mask << cmBitsPerColor));
						if (destPix == 0) {
							mapIndex = 1;
							goto l1;
						}
						mapIndex = destPix;
						goto l1;
					}
				l1:	/* end rgbMap:from:to: */;
				} else {
					/* begin rgbMap:from:to: */
					if ((d1 = cmBitsPerColor - 8) > 0) {
						mask3 = (1 << 8) - 1;
						srcPix1 = pixVal << d1;
						mask3 = mask3 << d1;
						destPix1 = srcPix1 & mask3;
						mask3 = mask3 << cmBitsPerColor;
						srcPix1 = srcPix1 << d1;
						mapIndex = (destPix1 + (srcPix1 & mask3)) + ((srcPix1 << d1) & (mask3 << cmBitsPerColor));
						goto l2;
					} else {
						if (d1 == 0) {
							mapIndex = pixVal;
							goto l2;
						}
						if (pixVal == 0) {
							mapIndex = pixVal;
							goto l2;
						}
						d1 = 8 - cmBitsPerColor;
						mask3 = (1 << cmBitsPerColor) - 1;
						srcPix1 = ((unsigned) pixVal) >> d1;
						destPix1 = srcPix1 & mask3;
						mask3 = mask3 << cmBitsPerColor;
						srcPix1 = ((unsigned) srcPix1) >> d1;
						destPix1 = (destPix1 + (srcPix1 & mask3)) + ((((unsigned) srcPix1) >> d1) & (mask3 << cmBitsPerColor));
						if (destPix1 == 0) {
							mapIndex = 1;
							goto l2;
						}
						mapIndex = destPix1;
						goto l2;
					}
				l2:	/* end rgbMap:from:to: */;
				}
			}
			longAtput(((((char *) colorMap)) + 4) + (mapIndex << 2), (longAt(((((char *) colorMap)) + 4) + (mapIndex << 2))) + 1);
		}
		maskShifted = ((unsigned) maskShifted) >> destPixSize;
		destShifted = ((unsigned) destShifted) >> destPixSize;
	}
	return destinationWord;
}

int targetForm(void) {
	return destForm;
}

void transferfromto(int count, int src, int dst) {
    int lastIn;
    int in;
    int out;

	in = src - 4;
	lastIn = in + (count * 4);
	out = dst - 4;
	while (in < lastIn) {
		longAtput(out += 4, longAt(in += 4));
	}
}

void transferfromIndexofObjecttoIndexofObject(int count, int firstFrom, int fromOop, int firstTo, int toOop) {
    int toIndex;
    int fromIndex;
    int lastFrom;

	fromIndex = fromOop + (firstFrom * 4);
	toIndex = toOop + (firstTo * 4);
	lastFrom = fromIndex + (count * 4);
	while (fromIndex < lastFrom) {
		fromIndex += 4;
		toIndex += 4;
		longAtput(toIndex, longAt(fromIndex));
	}
}

void transferTo(int aProc) {
    int sched;
    int newProc;
    int oldProc;
    int valuePointer;
    int tmp;

	newProc = aProc;
	sched = longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2));
	oldProc = longAt(((((char *) sched)) + 4) + (1 << 2));
	/* begin storePointer:ofObject:withValue: */
	valuePointer = activeContext;
	if (oldProc < youngStart) {
		possibleRootStoreIntovalue(oldProc, valuePointer);
	}
	longAtput(((((char *) oldProc)) + 4) + (1 << 2), valuePointer);
	/* begin storePointer:ofObject:withValue: */
	if (sched < youngStart) {
		possibleRootStoreIntovalue(sched, newProc);
	}
	longAtput(((((char *) sched)) + 4) + (1 << 2), newProc);
	/* begin newActiveContext: */
	/* begin storeContextRegisters: */
	longAtput(((((char *) activeContext)) + 4) + (1 << 2), ((((instructionPointer - method) - (4 - 2)) << 1) | 1));
	longAtput(((((char *) activeContext)) + 4) + (2 << 2), (((((((unsigned) ((stackPointer - activeContext) - 4)) >> 2) - 6) + 1) << 1) | 1));
	if ((longAt(((((char *) newProc)) + 4) + (1 << 2))) < youngStart) {
		beRootIfOld(longAt(((((char *) newProc)) + 4) + (1 << 2)));
	}
	activeContext = longAt(((((char *) newProc)) + 4) + (1 << 2));
	/* begin fetchContextRegisters: */
	tmp = longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (3 << 2));
	if ((tmp & 1)) {
		tmp = longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (5 << 2));
		if (tmp < youngStart) {
			beRootIfOld(tmp);
		}
	} else {
		tmp = longAt(((((char *) newProc)) + 4) + (1 << 2));
	}
	theHomeContext = tmp;
	receiver = longAt(((((char *) tmp)) + 4) + (5 << 2));
	method = longAt(((((char *) tmp)) + 4) + (3 << 2));
	tmp = ((longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (1 << 2))) >> 1);
	instructionPointer = ((method + tmp) + 4) - 2;
	tmp = ((longAt(((((char *) (longAt(((((char *) newProc)) + 4) + (1 << 2))))) + 4) + (2 << 2))) >> 1);
	stackPointer = ((longAt(((((char *) newProc)) + 4) + (1 << 2))) + 4) + (((6 + tmp) - 1) * 4);
	reclaimableContextCount = 0;
}

int tryCopyingBitsQuickly(void) {
	if (noSource) {
		return 0;
	}
	if (!(combinationRule == 34)) {
		return 0;
	}
	if (!(sourcePixSize == 32)) {
		return 0;
	}
	if (sourceForm == destForm) {
		return 0;
	}
	if (destPixSize < 8) {
		return 0;
	}
	if ((destPixSize == 8) && (colorMap == nilObj)) {
		return 0;
	}
	if (destPixSize == 32) {
		alphaSourceBlendBits32();
	}
	if (destPixSize == 16) {
		alphaSourceBlendBits16();
	}
	if (destPixSize == 8) {
		alphaSourceBlendBits8();
	}
	affectedL = dx;
	affectedR = dx + bbW;
	affectedT = dy;
	affectedB = dy + bbH;
	return 1;
}

void unPop(int nItems) {
	stackPointer += nItems * 4;
}

void unknownBytecode(void) {
	error("Unknown bytecode");
}

int upward(void) {
    int header;
    int type;

	if ((parentField & 1) == 1) {
		if (parentField == 3) {
			header = (longAt(field)) & 4294967292U;
			/* begin rightType: */
			if ((header & 252) == 0) {
				type = 0;
				goto l1;
			} else {
				if ((header & 126976) == 0) {
					type = 1;
					goto l1;
				} else {
					type = 3;
					goto l1;
				}
			}
		l1:	/* end rightType: */;
			longAtput(field, header + type);
			return 4;
		} else {
			child = field;
			field = parentField - 1;
			parentField = longAt(field);
			header = longAt(field + 4);
			/* begin rightType: */
			if ((header & 252) == 0) {
				type = 0;
				goto l2;
			} else {
				if ((header & 126976) == 0) {
					type = 1;
					goto l2;
				} else {
					type = 3;
					goto l2;
				}
			}
		l2:	/* end rightType: */;
			longAtput(field, child + type);
			field += 4;
			header = header & 4294967292U;
			longAtput(field, header + type);
			return 3;
		}
	} else {
		child = field;
		field = parentField;
		parentField = longAt(field);
		longAtput(field, child);
		field -= 4;
		return 1;
	}
}

int wakeHighestPriority(void) {
    int schedLists;
    int processList;
    int p;
    int sz;
    int header;

	schedLists = longAt(((((char *) (longAt(((((char *) (longAt(((((char *) specialObjectsOop)) + 4) + (3 << 2))))) + 4) + (1 << 2))))) + 4) + (0 << 2));
	/* begin fetchWordLengthOf: */
	/* begin sizeBitsOf: */
	header = longAt(schedLists);
	if ((header & 3) == 0) {
		sz = (longAt(schedLists - 8)) & 4294967292U;
		goto l1;
	} else {
		sz = header & 252;
		goto l1;
	}
l1:	/* end sizeBitsOf: */;
	p = ((unsigned) (sz - 4)) >> 2;
	p -= 1;
	processList = longAt(((((char *) schedLists)) + 4) + (p << 2));
	while ((longAt(((((char *) processList)) + 4) + (0 << 2))) == nilObj) {
		p -= 1;
		if (p < 0) {
			error("scheduler could not find a runnable process");
		}
		processList = longAt(((((char *) schedLists)) + 4) + (p << 2));
	}
	return removeFirstLinkOfList(processList);
}

int warpBits(void) {
    int ns;
    int skewWord;
    int mergeWord;
    int startBits;
    int yDelta;
    int smoothingCount;
    int sourceMapOop;
    int t;
    int i;
    int nSteps;
    int word;
    int halftoneWord;
    int deltaP12x;
    int deltaP12y;
    int deltaP43x;
    int deltaP43y;
    int pAx;
    int pAy;
    int pBx;
    int xDelta;
    int pBy;
    int integerPointer;

	ns = noSource;
	noSource = 1;
	clipRange();
	noSource = ns;
	if (noSource || ((bbW <= 0) || (bbH <= 0))) {
		affectedL = affectedR = affectedT = affectedB = 0;
		return null;
	}
	destMaskAndPointerInit();
	/* begin warpLoop */
	if (!((fetchWordLengthOf(bitBltOop)) >= (15 + 12))) {
		successFlag = 0;
		goto l2;
	}
	nSteps = height - 1;
	if (nSteps <= 0) {
		nSteps = 1;
	}
	pAx = fetchIntegerOrTruncFloatofObject(15, bitBltOop);
	t = fetchIntegerOrTruncFloatofObject(15 + 3, bitBltOop);
	deltaP12x = deltaFromtonSteps(pAx, t, nSteps);
	if (deltaP12x < 0) {
		pAx = t - (nSteps * deltaP12x);
	}
	pAy = fetchIntegerOrTruncFloatofObject(15 + 1, bitBltOop);
	t = fetchIntegerOrTruncFloatofObject(15 + 4, bitBltOop);
	deltaP12y = deltaFromtonSteps(pAy, t, nSteps);
	if (deltaP12y < 0) {
		pAy = t - (nSteps * deltaP12y);
	}
	pBx = fetchIntegerOrTruncFloatofObject(15 + 9, bitBltOop);
	t = fetchIntegerOrTruncFloatofObject(15 + 6, bitBltOop);
	deltaP43x = deltaFromtonSteps(pBx, t, nSteps);
	if (deltaP43x < 0) {
		pBx = t - (nSteps * deltaP43x);
	}
	pBy = fetchIntegerOrTruncFloatofObject(15 + 10, bitBltOop);
	t = fetchIntegerOrTruncFloatofObject(15 + 7, bitBltOop);
	deltaP43y = deltaFromtonSteps(pBy, t, nSteps);
	if (deltaP43y < 0) {
		pBy = t - (nSteps * deltaP43y);
	}
	if (!successFlag) {
		goto l2;
	}
	if (argumentCount == 2) {
		/* begin stackIntegerValue: */
		integerPointer = longAt(stackPointer - (1 * 4));
		if ((integerPointer & 1)) {
			smoothingCount = (integerPointer >> 1);
			goto l1;
		} else {
			successFlag = 0;
			smoothingCount = 0;
			goto l1;
		}
	l1:	/* end stackIntegerValue: */;
		sourceMapOop = longAt(stackPointer - (0 * 4));
		if (sourceMapOop == nilObj) {
			if (sourcePixSize < 16) {
				successFlag = 0;
				goto l2;
			}
		} else {
			if ((fetchWordLengthOf(sourceMapOop)) < (1 << sourcePixSize)) {
				successFlag = 0;
				goto l2;
			}
		}
	} else {
		smoothingCount = 1;
		sourceMapOop = nilObj;
	}
	startBits = pixPerWord - (dx & (pixPerWord - 1));
	nSteps = width - 1;
	if (nSteps <= 0) {
		nSteps = 1;
	}
	for (i = destY; i <= (clipY - 1); i += 1) {
		pAx += deltaP12x;
		pAy += deltaP12y;
		pBx += deltaP43x;
		pBy += deltaP43y;
	}
	for (i = 1; i <= bbH; i += 1) {
		xDelta = deltaFromtonSteps(pAx, pBx, nSteps);
		if (xDelta >= 0) {
			sx = pAx;
		} else {
			sx = pBx - (nSteps * xDelta);
		}
		yDelta = deltaFromtonSteps(pAy, pBy, nSteps);
		if (yDelta >= 0) {
			sy = pAy;
		} else {
			sy = pBy - (nSteps * yDelta);
		}
		for (word = destX; word <= (clipX - 1); word += 1) {
			sx += xDelta;
			sy += yDelta;
		}
		if (noHalftone) {
			halftoneWord = 4294967295U;
		} else {
			halftoneWord = longAt(halftoneBase + ((((dy + i) - 1) % halftoneHeight) * 4));
		}
		destMask = mask1;
		if (bbW < startBits) {
			skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(bbW, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
			skewWord = ((((startBits - bbW) * destPixSize) < 0) ? ((unsigned) skewWord >> -((startBits - bbW) * destPixSize)) : ((unsigned) skewWord << ((startBits - bbW) * destPixSize)));
		} else {
			skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(startBits, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
		}
		for (word = 1; word <= nWords; word += 1) {
			mergeWord = mergewith(skewWord & halftoneWord, (longAt(destIndex)) & destMask);
			longAtput(destIndex, (destMask & mergeWord) | ((~destMask) & (longAt(destIndex))));
			destIndex += 4;
			if (word >= (nWords - 1)) {
				if (!(word == nWords)) {
					destMask = mask2;
					skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(pixPerWord, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
				}
			} else {
				destMask = 4294967295U;
				skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(pixPerWord, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
			}
		}
		pAx += deltaP12x;
		pAy += deltaP12y;
		pBx += deltaP43x;
		pBy += deltaP43y;
		destIndex += destDelta;
	}
l2:	/* end warpLoop */;
	if (hDir > 0) {
		affectedL = dx;
		affectedR = dx + bbW;
	} else {
		affectedL = (dx - bbW) + 1;
		affectedR = dx + 1;
	}
	if (vDir > 0) {
		affectedT = dy;
		affectedB = dy + bbH;
	} else {
		affectedT = (dy - bbH) + 1;
		affectedB = dy + 1;
	}
}

int warpLoop(void) {
    int skewWord;
    int mergeWord;
    int startBits;
    int yDelta;
    int smoothingCount;
    int sourceMapOop;
    int t;
    int i;
    int nSteps;
    int word;
    int halftoneWord;
    int deltaP12x;
    int deltaP12y;
    int deltaP43x;
    int deltaP43y;
    int pAx;
    int pAy;
    int pBx;
    int xDelta;
    int pBy;
    int integerPointer;

	if (!((fetchWordLengthOf(bitBltOop)) >= (15 + 12))) {
		return successFlag = 0;
	}
	nSteps = height - 1;
	if (nSteps <= 0) {
		nSteps = 1;
	}
	pAx = fetchIntegerOrTruncFloatofObject(15, bitBltOop);
	t = fetchIntegerOrTruncFloatofObject(15 + 3, bitBltOop);
	deltaP12x = deltaFromtonSteps(pAx, t, nSteps);
	if (deltaP12x < 0) {
		pAx = t - (nSteps * deltaP12x);
	}
	pAy = fetchIntegerOrTruncFloatofObject(15 + 1, bitBltOop);
	t = fetchIntegerOrTruncFloatofObject(15 + 4, bitBltOop);
	deltaP12y = deltaFromtonSteps(pAy, t, nSteps);
	if (deltaP12y < 0) {
		pAy = t - (nSteps * deltaP12y);
	}
	pBx = fetchIntegerOrTruncFloatofObject(15 + 9, bitBltOop);
	t = fetchIntegerOrTruncFloatofObject(15 + 6, bitBltOop);
	deltaP43x = deltaFromtonSteps(pBx, t, nSteps);
	if (deltaP43x < 0) {
		pBx = t - (nSteps * deltaP43x);
	}
	pBy = fetchIntegerOrTruncFloatofObject(15 + 10, bitBltOop);
	t = fetchIntegerOrTruncFloatofObject(15 + 7, bitBltOop);
	deltaP43y = deltaFromtonSteps(pBy, t, nSteps);
	if (deltaP43y < 0) {
		pBy = t - (nSteps * deltaP43y);
	}
	if (!successFlag) {
		return 0;
	}
	if (argumentCount == 2) {
		/* begin stackIntegerValue: */
		integerPointer = longAt(stackPointer - (1 * 4));
		if ((integerPointer & 1)) {
			smoothingCount = (integerPointer >> 1);
			goto l1;
		} else {
			successFlag = 0;
			smoothingCount = 0;
			goto l1;
		}
	l1:	/* end stackIntegerValue: */;
		sourceMapOop = longAt(stackPointer - (0 * 4));
		if (sourceMapOop == nilObj) {
			if (sourcePixSize < 16) {
				return successFlag = 0;
			}
		} else {
			if ((fetchWordLengthOf(sourceMapOop)) < (1 << sourcePixSize)) {
				return successFlag = 0;
			}
		}
	} else {
		smoothingCount = 1;
		sourceMapOop = nilObj;
	}
	startBits = pixPerWord - (dx & (pixPerWord - 1));
	nSteps = width - 1;
	if (nSteps <= 0) {
		nSteps = 1;
	}
	for (i = destY; i <= (clipY - 1); i += 1) {
		pAx += deltaP12x;
		pAy += deltaP12y;
		pBx += deltaP43x;
		pBy += deltaP43y;
	}
	for (i = 1; i <= bbH; i += 1) {
		xDelta = deltaFromtonSteps(pAx, pBx, nSteps);
		if (xDelta >= 0) {
			sx = pAx;
		} else {
			sx = pBx - (nSteps * xDelta);
		}
		yDelta = deltaFromtonSteps(pAy, pBy, nSteps);
		if (yDelta >= 0) {
			sy = pAy;
		} else {
			sy = pBy - (nSteps * yDelta);
		}
		for (word = destX; word <= (clipX - 1); word += 1) {
			sx += xDelta;
			sy += yDelta;
		}
		if (noHalftone) {
			halftoneWord = 4294967295U;
		} else {
			halftoneWord = longAt(halftoneBase + ((((dy + i) - 1) % halftoneHeight) * 4));
		}
		destMask = mask1;
		if (bbW < startBits) {
			skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(bbW, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
			skewWord = ((((startBits - bbW) * destPixSize) < 0) ? ((unsigned) skewWord >> -((startBits - bbW) * destPixSize)) : ((unsigned) skewWord << ((startBits - bbW) * destPixSize)));
		} else {
			skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(startBits, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
		}
		for (word = 1; word <= nWords; word += 1) {
			mergeWord = mergewith(skewWord & halftoneWord, (longAt(destIndex)) & destMask);
			longAtput(destIndex, (destMask & mergeWord) | ((~destMask) & (longAt(destIndex))));
			destIndex += 4;
			if (word >= (nWords - 1)) {
				if (!(word == nWords)) {
					destMask = mask2;
					skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(pixPerWord, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
				}
			} else {
				destMask = 4294967295U;
				skewWord = warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(pixPerWord, xDelta, yDelta, deltaP12x, deltaP12y, smoothingCount, sourceMapOop);
			}
		}
		pAx += deltaP12x;
		pAy += deltaP12y;
		pBx += deltaP43x;
		pBy += deltaP43y;
		destIndex += destDelta;
	}
}

int warpSourcePixelsxDeltahyDeltahxDeltavyDeltavsmoothingsourceMap(int nPix, int xDeltah, int yDeltah, int xDeltav, int yDeltav, int n, int sourceMapOop) {
    int destWord;
    int sourcePix;
    int sourcePixMask;
    int destPixMask;
    int srcPixPerWord;
    int destPix;
    int i;
    int mask;
    int srcPix;
    int destPix1;
    int d;
    int mask3;
    int srcPix1;
    int destPix2;
    int d1;
    int mask4;
    int srcPix2;
    int destPix3;
    int d2;
    int mask5;
    int srcPix3;
    int destPix4;
    int d3;

	sourcePixMask = maskTable[sourcePixSize];
	destPixMask = maskTable[destPixSize];
	srcPixPerWord = 32 / sourcePixSize;
	destWord = 0;
	for (i = 1; i <= nPix; i += 1) {
		if (n > 1) {
			destPix = (smoothPixatXfyfdxhdyhdxvdyvpixPerWordpixelMasksourceMap(n, sx, sy, xDeltah / n, yDeltah / n, xDeltav / n, yDeltav / n, srcPixPerWord, sourcePixMask, sourceMapOop)) & destPixMask;
		} else {
			sourcePix = (sourcePixAtXypixPerWord(((unsigned) sx) >> 14, ((unsigned) sy) >> 14, srcPixPerWord)) & sourcePixMask;
			if (colorMap == nilObj) {
				if (destPixSize == sourcePixSize) {
					destPix = sourcePix;
				} else {
					if (sourcePixSize >= 16) {
						if (sourcePixSize == 16) {
							/* begin rgbMap:from:to: */
							if ((d = 8 - 5) > 0) {
								mask = (1 << 5) - 1;
								srcPix = sourcePix << d;
								mask = mask << d;
								destPix1 = srcPix & mask;
								mask = mask << 8;
								srcPix = srcPix << d;
								destPix = (destPix1 + (srcPix & mask)) + ((srcPix << d) & (mask << 8));
								goto l1;
							} else {
								if (d == 0) {
									destPix = sourcePix;
									goto l1;
								}
								if (sourcePix == 0) {
									destPix = sourcePix;
									goto l1;
								}
								d = 5 - 8;
								mask = (1 << 8) - 1;
								srcPix = ((unsigned) sourcePix) >> d;
								destPix1 = srcPix & mask;
								mask = mask << 8;
								srcPix = ((unsigned) srcPix) >> d;
								destPix1 = (destPix1 + (srcPix & mask)) + ((((unsigned) srcPix) >> d) & (mask << 8));
								if (destPix1 == 0) {
									destPix = 1;
									goto l1;
								}
								destPix = destPix1;
								goto l1;
							}
						l1:	/* end rgbMap:from:to: */;
						} else {
							/* begin rgbMap:from:to: */
							if ((d1 = 5 - 8) > 0) {
								mask3 = (1 << 8) - 1;
								srcPix1 = sourcePix << d1;
								mask3 = mask3 << d1;
								destPix2 = srcPix1 & mask3;
								mask3 = mask3 << 5;
								srcPix1 = srcPix1 << d1;
								destPix = (destPix2 + (srcPix1 & mask3)) + ((srcPix1 << d1) & (mask3 << 5));
								goto l2;
							} else {
								if (d1 == 0) {
									destPix = sourcePix;
									goto l2;
								}
								if (sourcePix == 0) {
									destPix = sourcePix;
									goto l2;
								}
								d1 = 8 - 5;
								mask3 = (1 << 5) - 1;
								srcPix1 = ((unsigned) sourcePix) >> d1;
								destPix2 = srcPix1 & mask3;
								mask3 = mask3 << 5;
								srcPix1 = ((unsigned) srcPix1) >> d1;
								destPix2 = (destPix2 + (srcPix1 & mask3)) + ((((unsigned) srcPix1) >> d1) & (mask3 << 5));
								if (destPix2 == 0) {
									destPix = 1;
									goto l2;
								}
								destPix = destPix2;
								goto l2;
							}
						l2:	/* end rgbMap:from:to: */;
						}
					} else {
						destPix = sourcePix & destPixMask;
					}
				}
			} else {
				if (sourcePixSize >= 16) {
					if (sourcePixSize == 16) {
						/* begin rgbMap:from:to: */
						if ((d2 = cmBitsPerColor - 5) > 0) {
							mask4 = (1 << 5) - 1;
							srcPix2 = sourcePix << d2;
							mask4 = mask4 << d2;
							destPix3 = srcPix2 & mask4;
							mask4 = mask4 << cmBitsPerColor;
							srcPix2 = srcPix2 << d2;
							sourcePix = (destPix3 + (srcPix2 & mask4)) + ((srcPix2 << d2) & (mask4 << cmBitsPerColor));
							goto l3;
						} else {
							if (d2 == 0) {
								sourcePix = sourcePix;
								goto l3;
							}
							if (sourcePix == 0) {
								sourcePix = sourcePix;
								goto l3;
							}
							d2 = 5 - cmBitsPerColor;
							mask4 = (1 << cmBitsPerColor) - 1;
							srcPix2 = ((unsigned) sourcePix) >> d2;
							destPix3 = srcPix2 & mask4;
							mask4 = mask4 << cmBitsPerColor;
							srcPix2 = ((unsigned) srcPix2) >> d2;
							destPix3 = (destPix3 + (srcPix2 & mask4)) + ((((unsigned) srcPix2) >> d2) & (mask4 << cmBitsPerColor));
							if (destPix3 == 0) {
								sourcePix = 1;
								goto l3;
							}
							sourcePix = destPix3;
							goto l3;
						}
					l3:	/* end rgbMap:from:to: */;
					} else {
						/* begin rgbMap:from:to: */
						if ((d3 = cmBitsPerColor - 8) > 0) {
							mask5 = (1 << 8) - 1;
							srcPix3 = sourcePix << d3;
							mask5 = mask5 << d3;
							destPix4 = srcPix3 & mask5;
							mask5 = mask5 << cmBitsPerColor;
							srcPix3 = srcPix3 << d3;
							sourcePix = (destPix4 + (srcPix3 & mask5)) + ((srcPix3 << d3) & (mask5 << cmBitsPerColor));
							goto l4;
						} else {
							if (d3 == 0) {
								sourcePix = sourcePix;
								goto l4;
							}
							if (sourcePix == 0) {
								sourcePix = sourcePix;
								goto l4;
							}
							d3 = 8 - cmBitsPerColor;
							mask5 = (1 << cmBitsPerColor) - 1;
							srcPix3 = ((unsigned) sourcePix) >> d3;
							destPix4 = srcPix3 & mask5;
							mask5 = mask5 << cmBitsPerColor;
							srcPix3 = ((unsigned) srcPix3) >> d3;
							destPix4 = (destPix4 + (srcPix3 & mask5)) + ((((unsigned) srcPix3) >> d3) & (mask5 << cmBitsPerColor));
							if (destPix4 == 0) {
								sourcePix = 1;
								goto l4;
							}
							sourcePix = destPix4;
							goto l4;
						}
					l4:	/* end rgbMap:from:to: */;
					}
				}
				destPix = (longAt(((((char *) colorMap)) + 4) + (sourcePix << 2))) & destPixMask;
			}
		}
		if (destPixSize == 32) {
			destWord = destPix;
		} else {
			destWord = (destWord << destPixSize) | destPix;
		}
		sx += xDeltah;
		sy += yDeltah;
	}
	return destWord;
}
