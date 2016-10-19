//+File: commlib.c
//
// UDP communication library
//
// Tested on Linux and Win32
//-

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef _WIN32

#include "winsock2.h"
typedef unsigned int socklen_t;
#undef NO_ERROR

#else

#include <fcntl.h>	// fnctl()

#include <time.h>		// clock()
#include <sys/time.h>	// gettimeofday()
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#endif //_WIN32

#include "base/errlib.h"
#include "commlib.h"

int DumpMemory(unsigned char *buf, int len);
void my_sleep(int usec);
void my_sleep2(void);

/*
 * Library-wide variables for BCU address
 * Most of these will probably be MACRO-ed away to some configuration routine
 * BCU_PORT, BCU_LOCAL_PORT are overridden with a call to UDPsetup
 */
static int BCU_socket = 0;
static int BCU_PORT = 10000;		
static int BCU_LOCAL_PORT = 10000;

// Library-wide msgID counter (only 8 bits wide!)
static unsigned char extern_msgID = 0;

// Debug output
static int debug = 0;

// Packets counter
static int packets_in_flight = 0;

// # of times to repeat revfrom() in case of EAGAIN error
int EAGAIN_LOOP = 5;

//+Function: USLEEP
//
// Implementation of usleep using nanosleep
//
//-

static void USLEEP(useconds_t usec)
{
struct timespec delay;

delay.tv_sec=usec/1000000;
delay.tv_nsec=(usec-(delay.tv_sec*1000000))*1000;
nanosleep(&delay,NULL);
}


//+Function: UDPsetup
//
// Sets up the UDP communication parameters. Right now there are only two parameters to set:
//
// BCU_PORT: the remote port the BCU is listening on
// BCU_LOCAL_PORT: the local port the BCU is answering to
//
// This function should be called before any UDP activity starts. Otherwise, the communication
// will use default parameters (both ports set to 10000)
//-
int UDPsetup(int local_port,  // local port
             int remote_port) // remote port
            
{
   BCU_PORT = remote_port;
   BCU_LOCAL_PORT = local_port;
   
   if(debug) {
   		printf("Initialized UDP (local-port:%d, remote-port:%d)\n", BCU_LOCAL_PORT, BCU_PORT);
   }	
	
   return NO_ERROR;
}

int UDPsetupSocket(int local_port,  // local port
                   int remote_port, // remote_port
                   int *socket)     // socket storage
            
{
   int err;

   BCU_PORT = remote_port;
   BCU_LOCAL_PORT = local_port;
   
   if(debug) {
   		printf("Initialized UDP (local-port:%d, remote-port:%d)\n", BCU_LOCAL_PORT, BCU_PORT);
   }	

   if (socket == NULL)
      return NETWORK_ERROR;

   if((err = CreateUDPsocket(local_port, socket)) != NO_ERROR) {
        printf("Error in CreateUDPsocket\n");
        return err;
        }

	
   return NO_ERROR;
}

//+Function: WaitUDPpacket
//
// Waits for an UDP packet
//
// This routine waits for an UDP packet to arrive at the specified port.
// Prior to the first call, <token> must point to an integer variable set to 0. The token will be

//+Function: WaitUDPpacket
//
// Waits for an UDP packet
//
// This routine waits for an UDP packet to arrive at the specified port.
// Prior to the first call, <token> must point to an integer variable set to 0. The token will be
// modified by the routine, and must not be modified from the external program. Subsequent
// calls must pass a pointer to the same variable. In these calls, the port number is ignored.
// This permits the routine to set up the network structures only one time.
// If, for some reason, the port number change, the token must be set to 0 again.
//
// Note: using a token created by CreateUDPsocket() means that port/hostname addresses will always be ignored. 
//
// Return value: the number of received bytes for success, or an error code for failure.
// In case of success, the buffer is filled with packet data, up to <bufferlen> bytes.
//-
int WaitUDPpacket(
	       	int port,					// port to wait on 
	       	char *buffer,				// buffer to receive data
	       	int bufferlen,				// max. buffer length
	       	int *token,					// connection token - must be a pointer to a zero integer the first time
			int timeout,				// timeout in milliseconds (zero = forever)
			struct sockaddr_in *from)	// filled with client information
{
	int err,i;
	socklen_t fromlen = sizeof(struct sockaddr_in);
	fd_set set;
	struct timeval tv;
	unsigned int *dummy;

	// A zero token means that we must set up the network
	if (*token == 0)
		if((err = CreateUDPsocket(port, token)) != NO_ERROR) {
			printf("Error in CreateUDPsocket\n");
			return err;
		}

	// Use a select() call to have a reliable timeout
	FD_ZERO(&set);
	FD_SET( (unsigned int)*token, &set);

	tv.tv_sec = timeout/1000;
	tv.tv_usec = timeout*1000;

	if (debug)
		printf("timeout =%d\n", timeout);

	err = select(*token+1, &set, NULL, NULL, (timeout==0) ? NULL : &tv);
	if (!err) {
		printf("Returning TIMEOUT ERROR\n");
		return PLAIN_ERROR(TIMEOUT_ERROR);
	}
	if (err<0) {
		perror("Error in select()");
	}

	// On Windows, it's necessary to null <fromlen> too
	dummy = &fromlen;
	if (from == NULL)
		dummy = NULL;

	// Loop trapping EAGAIN errors
	for (i=0; i<EAGAIN_LOOP; i++) {
		if ((err = recvfrom(*token, buffer, bufferlen, 0, (struct sockaddr *)from, dummy)) >=0) {
			packets_in_flight--;	// [del] This can be removed in the final version of new the MirrorCtrl

			if (debug) {
				printf("WaitUDPpacket(): Recvfrom: received %d bytes\n", err);
				printf("Returning %d\n", err);
			}
			return err;
		}
		// If we got an error, loop again in case of an EAGAIN
		else {
			if(errno == EAGAIN) {
                fprintf(stderr, "WaitUDPpacket(): received %d bytes instead of %d\n", err, bufferlen);
                fprintf(stderr, "WaitUDPpacket(): Recovering from EAGAIN error in recvfrom() - iteration %d\n", i);
                USLEEP(20);
				continue;
			}

#ifdef _WIN32
			int rc = WSAGetLastError();
			return -rc;
#else
			perror("recvfrom");
			return SYSTEM_ERROR(NETWORK_ERROR);
#endif
		}
	}

	//fprintf(stderr, "WaitUDPpacket(): EAGAIN repetead %d times, exiting\n", EAGAIN_LOOP);
	return SYSTEM_ERROR(NETWORK_ERROR);
}	

//+Function: SendUDPpacket()
//
// Writes data using UDP sockets
//
// This function sets up an UDP socket and use it to send a remote host 
// a data buffer. 
// The remote host must be specified using an inet format address 
//-
int SendUDPpacket(char *buffer,				// Pointer to buffer of data
	       		  int bufferlen,			// buffer length in bytes
				  unsigned int remote_addr,	// Remote address in inet format 
				  int remote_port,			// Remote port
				  int *socket_out)			// pointer to socket to use (optional, set to 0 if no existing sockets are to be used)	
{
	int rc, stat;

	struct sockaddr_in remoteServAddr;

	remoteServAddr.sin_family=AF_INET;
	remoteServAddr.sin_port=htons( (unsigned short)remote_port);
	remoteServAddr.sin_addr.s_addr = remote_addr;

	packets_in_flight++; // [del] This can be removed in the final version of new the MirrorCtrl

	if (debug) printf("SendUDPpacket(): Sending packet to %s %d, len=%d\n", inet_ntoa(remoteServAddr.sin_addr), remote_port, bufferlen);

	// Create a permanent socket, if we don't have one
	if (*socket_out == 0)
		{
		if (debug) fprintf( stderr, "SendUDPpacket(): Creating socket...\n");
		if ((stat = CreateUDPsocket(BCU_PORT, socket_out)))
			{
			printf("Error in CreateUDPsocket\n");
			return stat;
			}
		}

	rc = sendto(*socket_out, buffer, bufferlen, 0, (struct sockaddr *) &remoteServAddr, sizeof(remoteServAddr));
  	if (rc<0){
#ifdef _WIN32
		rc = WSAGetLastError();
		return -rc;
#else
		if (debug) {
			fprintf( stderr, "SendUDPpacket():  Error sending data...\n");
		}
		perror("sendto()");
		return SYSTEM_ERROR(NETWORK_ERROR);
#endif
	}

	return NO_ERROR;
}


/*
 * Wrapper on SendUDPpacket, but resolves host names
 */
int SendUDPpacketToHost(
	       	char *buffer,		// Pointer to buffer of data
	       	int bufferlen,		// buffer length in bytes
			char *remote_addr,	// Remote hostname or IP address
			int remote_port,	// Remote port
			int *socket_out)	// pointer to socket to use (optional, set to 0 if no existing sockets are to be used)


	{ return SendUDPpacket(buffer, bufferlen, inet_addr(remote_addr), remote_port, socket_out); }



//+Function: CreateUDPsocket
//
// Creates an UDP socket for future listening
//
// This routine creates an UDP socket, with the implicit assumption that the socket
// will be used in one or more subsequent calls to WaitUDPpacket().
// The <token> argument must point to an integer variable that will be used as a socket
// identifier by WaitUDPsocket(). The initial content of this variable is unimportant. 
//-
int CreateUDPsocket(int port,	// Port to wait on
		 			int *token)	// connection token
{
	int sock=0;
	socklen_t addrlen;

	struct sockaddr_in my_addr;

	my_addr.sin_family=AF_INET;
	my_addr.sin_port=htons((unsigned short)port);
	my_addr.sin_addr.s_addr = htonl(INADDR_ANY);

	addrlen = sizeof(my_addr);

	if (debug) { 
		printf("CreateUDPsocket(): Setting up listen socket on localhost,port %d\n", port);
	}

	// Create the socket
	sock = socket( AF_INET, SOCK_DGRAM, 0);
	if (sock == -1)
		{
		if (debug) perror("socket");
		return SYSTEM_ERROR(NETWORK_ERROR);
		}

	// Bind the socket to our local address
	if (bind( sock, (struct sockaddr *) &my_addr, addrlen) == -1){
		if (debug) {
			perror("bind");
		}
		return SYSTEM_ERROR(NETWORK_ERROR);
	}

	*token = sock;
	if (debug) {
		printf("CreateUDPsocket(): Socket created\n");
	}

	fcntl(sock, F_SETFL, O_NONBLOCK);
	perror("fnctl O_NONBLOCK");
	return NO_ERROR;
}


int DeleteUDPSocket(int token) {
	int res = close(token);
	if(debug) {
		printf("Error %d closing socket\n", errno);
	}
	return res;
}

// --------------------------------------------------- //
// --- FUNCTIONS NOT YET USED IN THE NEW MirrorCtr --- //
// --------------------------------------------------- //

//+Function: WriteBCUinternal
//
//	internal function to send a single packet with a write command
//-
int WriteBCUinternal(
		char *remote_addr,		// remote IP address in dotted format
		int firstDSP,			// First DSP to be addressed
		int lastDSP,			// Last DSP to be adressed
		unsigned int address,		// address in BCU memory
		BYTE *data,			// data buffer
		int datalen,			// data buffer length for each DSP (in 32 bit words)
		int command,			// Requested command: WR_SAME_DSP, WR_SEQ_DSP or WR_SCIMEASURE_RAM
		int flags,			// additional flags
		BCUcommand *packet_buffer,	// optional external storage for packet
		int *sent_len,			// optional external storage for packet length
		unsigned char msgID			// Message ID for this packet
		)
{
	int result;
	int single_len;
	unsigned char cmd[MAX_UDP_PACKET_LENGTH];
	BCUcommand *cmd2;

	int Hsize = sizeof(BCUcommand);

	// Write request check
//	if ((command != WR_SAME_DSP) && (command != WR_SEQ_DSP) && (command != WR_SCIMEASURE_RAM) && (command != WR_SRAM) && (command != WR_SDRAM))
//		return PLAIN_ERROR(ILLEGAL_MSG_ERROR);

	if (packet_buffer)
		cmd2 = packet_buffer;
	else
		cmd2 = (BCUcommand *)cmd;

	memset( cmd2, 0, sizeof(BCUcommand));

	// Fill command buffer
	SetFirstDSP(*cmd2, firstDSP);
	SetLastDSP(*cmd2, lastDSP);

	SetMemoryAddress(*cmd2, address);

	if (IsSeqOpcode(command))
		single_len = datalen / ((lastDSP-firstDSP)/GetDSPfactor(command)+1);
	else
		single_len = datalen;

	if (debug)
		printf("Length, single length: %d,%d\n", datalen, single_len);

	SetDataLength(*cmd2, single_len);
	SetCmdCode(*cmd2, command);
	SetFlag_wantreply( *cmd2, 1);
	SetMsgID( *cmd2, msgID);

	// Should convert this to a macro...
	if (flags & FLAG_ASQUADWORD)
		{
		cmd2->flags |= FLAG_ASQUADWORD;
		printf("WriteBCUinternal: using FLAG_ASQUADWORD\n");
		}
	
	if (debug)
		{
		printf("WriteBCUinternal(): Datalen: %d\n", datalen);
		printf("WriteBCUinternal(): Address: 0x%X\n", address);
		fprintf(stderr, "WriteBCUinternal(): First,LastDSP: %d,%d\n", firstDSP,lastDSP);
		}

	memcpy( (unsigned char *)cmd2+Hsize, data, datalen* sizeof(float32));

	if (debug)
		{
		printf("Dumping buffer\n");
		DumpMemory( (unsigned char *)cmd2+Hsize, datalen* sizeof(float32));
		}

	result = SendUDPpacketToHost( (char *)cmd2, Hsize+datalen*sizeof(float32), remote_addr, BCU_PORT, &BCU_socket);
	if (result != NO_ERROR)
		{
#ifdef _WIN32
		shutdown( BCU_socket, SD_BOTH);
#else
		shutdown( BCU_socket, 2);
#endif
		}

	if (sent_len)
		*sent_len = Hsize+datalen*sizeof(float32);

	return result;
}


//+Function: ReadBCUinternal
//
// internal function to send a single packet with read BCU memory request
//-

int ReadBCUinternal(
		char *remote_addr,		// remote IP address in dotted format
		int firstDSP,			// First DSP to be addressed
		int lastDSP,			// Last DSP to be addressed
		unsigned int address,		// address in BCU memory (in 32 bit words)
		int datalen,			// requested length (in 32 bit words for each DSP)
		int command,			// command opcode (RD_SEQ_DSP)
		int flags,			// additional flags
		BCUcommand *packet_buffer,	// optional storage for sent packet
		int *sent_len,			// length of sent packet
		unsigned char msgID			// messageID to use
		)
{
	BCUcommand cmd;
	BCUcommand *cmd2;

	int result;


	// Use external buffer if provided, otherwise our internal one
	if (packet_buffer)
		cmd2 = packet_buffer;
	else
		cmd2 = &cmd;

	memset( cmd2, 0, sizeof(BCUcommand));

	// Fill command buffer
	SetFirstDSP(*cmd2, firstDSP);
	SetLastDSP(*cmd2, lastDSP);
	SetMemoryAddress(*cmd2, address);

	SetDataLength(*cmd2, datalen);

	SetCmdCode(*cmd2, command);
	SetMsgID(*cmd2, msgID);
	SetFlag_wantreply(*cmd2, 1);

	// Should convert this to a macro...
	if (flags & FLAG_ASQUADWORD)
		cmd2->flags |= FLAG_ASQUADWORD;
	
	if (debug)
		{
		fprintf( stderr, "ReadBCUinternal: first,last DSP: %d,%d\n", firstDSP, lastDSP);
//		DumpMemory( (unsigned char *)cmd2, sizeof(BCUcommand));
		}

	// Send read request
	result = SendUDPpacketToHost( (char *)cmd2, sizeof(BCUcommand), remote_addr, BCU_PORT, &BCU_socket);
	if (result != NO_ERROR)
		{
#ifdef _WIN32
		shutdown( BCU_socket, SD_BOTH);
#else
		shutdown( BCU_socket, 2);
#endif

		}

	// tells the external routine how many bytes we sent
	if (sent_len)
		*sent_len = sizeof(BCUcommand);
	
	return result;
}

//+Function: ResendPacket
//
// Sends again a packet buffer
//-

int ResendPacket( BCUcommand *cmd, int buffer_len, char *remote_addr)
{
	return SendUDPpacketToHost( (char *)cmd, buffer_len, remote_addr, BCU_PORT, &BCU_socket);
}

//+Function: IsDataMemory
//
// tells if a given address is in the BCU data memory or not
//
//-

int IsDataMemory( int address)

{
	if ((address >= 0x1000000) && (address <=0x19000000))
		return 1;
	else
		return 0;
}



//+Function: GetPacketNum
//
// returns the number of packets that a read or write command would be split into
//
// <len> is the number of 32 bit words to be transferred
//-

int GetPacketNum( int opcode, int firstDSP, int lastDSP, int BCU_address, int len, unsigned char *firstID)
{
	int num=0;
	int toadd=0;
	int additionalDSP,numDSP;

	// We don't look at firstDSP/lastDSP, since this part is not yet implemented

	printf("Processing len = %d, BCUaddr = %d, MAX_DATA_TRANSFER = %d, first,last = %d,%d\n", len, BCU_address, MAX_DATA_TRANSFER, firstDSP, lastDSP);

	additionalDSP = (lastDSP-firstDSP)/GetDSPfactor(opcode);
	numDSP = (lastDSP-firstDSP)/GetDSPfactor(opcode)+1;

	// Start with the raw number of packets
	num = len / MAX_DATA_TRANSFER;
	if ( len % MAX_DATA_TRANSFER)
		num++;

	// Get the number of additional packets (leaving out the packets
	// already counted
//	toadd = (num * additionalDSP) - num;
	toadd=0;

	// Get the number of ADDITIONAL packets (thus, minus 1 DSP)
//	toadd = additionalDSP * ((len/MAX_DATA_TRANSFER) + ((len % MAX_DATA_TRANSFER==0)?0:1));

	printf("Adding %d for body\n", toadd);
	// One more packet if the start address is not aligned
	if ((len>=0x08) && ( BCU_address & 0x03))
		{
		int mylen;
		mylen = (4-(BCU_address & 0x03));
		num++;
		len = len - mylen;

		// Get the number of ADDITIONAL packets 
		// Number of DSPs / # of DSPs that fit into a packet, minus
		// the already counted packet

		
		toadd += (additionalDSP+1) / (MAX_DATA_TRANSFER/mylen) -1;
		printf("Adding to %d for start\n", toadd);
		}

	// One more packet if the end address is not aligned
	if ((len>=0x08) && (len & 0x03))
		{
		num++;
		// Get the number of ADDITIONAL packets (thus, minus 1 DSP)
		toadd += (additionalDSP+1) / (MAX_DATA_TRANSFER/(len&0x03)) -1;
		if (toadd<0) toadd=0;
		printf("Adding to %d for end\n", toadd);
		}

       printf("Num packets before mult. = %d\n", num);
	printf("len / MAX_DATA_TRANSFER = %d\n", len / MAX_DATA_TRANSFER);
	printf("len %% MAX_DATA_TRANSFER = %d\n", len % MAX_DATA_TRANSFER);
	printf("DSP factor: %d\n", GetDSPfactor(opcode));
	// Now take into account multiple DSP splitting
	if (IsSeqOpcode( opcode))
		{
		printf("GetPacketsNum: SEQ command\n");
		num += toadd;
		}

	if (firstID)
		*firstID = extern_msgID;

	return num;
}



void my_sleep2()
{
	int dummy=0;

	dummy++;
}

void my_sleep( int usec)
{
	int i,a=0;
	int num = usec * 250;

	for (i=0; i< num; i++)
		a++;

}

//+Function: IsReadOpcode
//
// Returns 1 if the opcode is a read command
//-

int IsReadOpcode( int opcode)
{
	if ((opcode == MGP_OP_RDSEQ_DSP) ||
		(opcode == MGP_OP_RDSEQ_FLASH) ||
		(opcode == MGP_OP_RDSEQ_SDRAM) ||
		(opcode == MGP_OP_RDSEQ_SRAM) ||
		(opcode == MGP_OP_RDSEQ_DIAGBUFF))
			return 1;
	
	return 0;

}

//+Function: IsWriteOpcode
//
// Returns 1 if the opcode is a read command.
// All "control" opcodes (e.g. lock flash, unlock flash, ect)
// are considered write opcodes
//-

int IsWriteOpcode( int opcode)
{
	if ((opcode == MGP_OP_RDSEQ_DSP) ||
		(opcode == MGP_OP_RDSEQ_FLASH) ||
		(opcode == MGP_OP_RDSEQ_SDRAM) ||
		(opcode == MGP_OP_RDSEQ_SRAM) ||
		(opcode == MGP_OP_RDSEQ_DIAGBUFF))
			return 0;
	
	return 1;

} 

//+Function: IsSameOpcode
//
// Returns 1 if the opcode is of the "SAME" family.
// All "control" opcodes (e.g. lock flash, unlock flash, ect)
// are considered SAME opcodes
//-

int IsSameOpcode( int opcode)
{
	if ((opcode == MGP_OP_WRSEQ_DSP) ||
		(opcode == MGP_OP_RDSEQ_DSP) ||
		(opcode == MGP_OP_RDSEQ_FLASH) ||
		(opcode == MGP_OP_WRSEQ_SDRAM) ||
		(opcode == MGP_OP_RDSEQ_SDRAM) ||
		(opcode == MGP_OP_WRSEQ_SRAM) ||
		(opcode == MGP_OP_RDSEQ_SRAM) ||
		(opcode == MGP_OP_WRSEQ_DIAGBUFF) ||
		(opcode == MGP_OP_RDSEQ_DIAGBUFF) ||
		(opcode == MGP_OP_WRRD_RELAIS_BOARD))
			return 0;
			
	return 1;
} 

//+Function: IsSeqOpcode
//
// Returns 1 if the opcode is of the "SEQ" family.
//-


int IsSeqOpcode( int opcode)
{
	if ((opcode == MGP_OP_WRSEQ_DSP) ||
		(opcode == MGP_OP_RDSEQ_DSP) ||
		(opcode == MGP_OP_RDSEQ_FLASH) ||
		(opcode == MGP_OP_WRSEQ_SDRAM) ||
		(opcode == MGP_OP_RDSEQ_SDRAM) ||
		(opcode == MGP_OP_WRSEQ_SRAM) ||
		(opcode == MGP_OP_RDSEQ_SRAM) ||
		(opcode == MGP_OP_WRSEQ_DIAGBUFF) ||
		(opcode == MGP_OP_RDSEQ_DIAGBUFF) ||
		(opcode == MGP_OP_WRRD_RELAIS_BOARD))

			return 1;
			
	return 0;
}

//+Function: GetDSPfactor
//
// Returns the number of DSP addressed by a single command, that is,
// the number to sum to a DSP number to get the next element.
//-


int GetDSPfactor( int opcode)
{
	if ((opcode == MGP_OP_WRSAME_DSP) ||
		(opcode == MGP_OP_WRSEQ_DSP) ||
		(opcode == MGP_OP_RDSEQ_DSP))
			return 1;
	
	return 2;
}

//@Function: WaitBCUanswer
//
// Waits a packet from the BCU
//
// It's actually a wrapper around the WaitUDPpacket() function
//@
 
int WaitBCUanswer(
		char *buffer,			// buffer where to place data
		int max_len,			// buffer length
		int timeout			// timeout in milliseconds
		)
{
	return WaitUDPpacket( BCU_LOCAL_PORT, buffer, max_len, &BCU_socket, timeout, NULL);
}


int DumpMemory( unsigned char *buf, int len)
{
	int i;
	float *buffloat = (float *)buf;

	for (i=0; i<len; i++)
		{	
		printf("%02X ", buf[i]);
		if ((i+1)%4 == 0)
			{
//			printf("  %5.2f", *(((float *)buf)+i/4));
			printf("\n");
			}
		}
	printf("\n");
	for (i=0; i<len/4; i++)
		{
		printf(" %5.2f ", buffloat[i]);
		}

	printf("\n");
		
	return 0;
}
