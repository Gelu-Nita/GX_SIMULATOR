#include <Windows.h>
#include <malloc.h>
#include "Render.h"

int ProcessorNumber;

typedef struct
{
 int NLOS;
 void *arg_global[23];
 int *Done;
 int res;
 HANDLE M;
} LOSlist;

DWORD WINAPI LOSThread(LPVOID arg)
{
 LOSlist *LL=(LOSlist*)arg;

 int AllDone=0;

 while (!AllDone)
 {
  int l=-1;

  WaitForSingleObject(LL->M, INFINITE);
  for (int i=0; i<LL->NLOS; i++) if (!LL->Done[i])
  {
   l=i;
   LL->Done[i]=1;
   break;
  }
  ReleaseMutex(LL->M);

  if (l<0) AllDone=1;
  else
  {
   void *ARGV[23];
   for (int i=0; i<23; i++) ARGV[i]=LL->arg_global[i];

   ARGV[6]=(void*)(((double*)ARGV[6])+l); //x1
   ARGV[7]=(void*)(((double*)ARGV[7])+l); //x2
   ARGV[8]=(void*)(((double*)ARGV[8])+l); //y1
   ARGV[9]=(void*)(((double*)ARGV[9])+l); //y2
   ARGV[10]=(void*)(((double*)ARGV[10])+l); //z1
   ARGV[11]=(void*)(((double*)ARGV[11])+l); //z2

   ARGV[15]=(void*)(((long*)ARGV[15])+l); //Nvoxels
   ARGV[16]=(void*)(((long*)ARGV[16])+l*arrN); //VoxList
   ARGV[17]=(void*)(((double*)ARGV[17])+l*arrN); //ds
   ARGV[18]=(void*)(((double*)ARGV[18])+l*arrN); //x_ind
   ARGV[19]=(void*)(((double*)ARGV[19])+l*arrN); //y_ind
   ARGV[20]=(void*)(((double*)ARGV[20])+l*arrN); //z_ind
   ARGV[21]=(void*)(((double*)ARGV[21])+l*3); //entry_point
   ARGV[22]=(void*)(((double*)ARGV[22])+l*3); //exit_point

   int r=RENDER(23, ARGV);
   if (r) LL->res=1;
  }
 }

 return 0;
}

extern "C" __declspec(dllexport) int RENDER_MULTI(int argc, void **argv)
{
 LOSlist LL;
 LL.NLOS=*((int*)argv[0]);
 for (int i=0; i<23; i++) LL.arg_global[i]=argv[i+1];

 LL.Done=(int*)malloc(sizeof(int)*LL.NLOS);
 for (int i=0; i<LL.NLOS; i++) LL.Done[i]=0;

 LL.res=0;
 LL.M=CreateMutexA(0, FALSE, 0);

 HANDLE* ThreadList=(HANDLE*)malloc(sizeof(HANDLE)*ProcessorNumber);

 for (int i=0; i<ProcessorNumber; i++) ThreadList[i]=CreateThread(0, 0, LOSThread, &LL, 0, 0);
 WaitForMultipleObjects(ProcessorNumber, ThreadList, TRUE, INFINITE);

 for (int i=0; i<ProcessorNumber; i++) CloseHandle(ThreadList[i]);
 free(ThreadList);

 CloseHandle(LL.M);
 free(LL.Done);

 return LL.res;
}