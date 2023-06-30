import ctypes
from numpy.ctypeslib import ndpointer
import numpy as np
import matplotlib.pyplot as plt
import h5py

def initGET_MW(libname):
	"""
	Python wrapper for fast gyrosynchrotron codes.
	Identical to GScodes.py in https://github.com/kuznetsov-radio/gyrosynchrotron
	This is for the single thread version
	@param libname: path for locating compiled shared library
	@return: An executable for calling the GS codes in single thread
	"""
	_intp = ndpointer(dtype=ctypes.c_int32, flags='F')
	_doublep = ndpointer(dtype=ctypes.c_double, flags='F')

	libc_mw = ctypes.CDLL(libname)
	mwfunc = libc_mw.PyGET_MW ### use PyGET_MW_USER for user defined zeta values
	mwfunc.argtypes = [_intp, _doublep, _doublep, _doublep, _doublep,_doublep,_doublep]
	mwfunc.restype = ctypes.c_int
	return mwfunc
    


libname='/home/surajit/GR-FF_general/GRFF_DEM_Transfer.so'
dem_file='/home/surajit/Downloads/20200927/aia_image_analysis/full_sun_dem_162300.hdf5'
outfile='Tbmap_dem.hdf5'
Nf=10    # number of frequencies
NSteps=1  # number of nodes along the line-of-sight
dx=4.8#arcsec
dy=4.8#arcsec

sfu2cgs=1e-19
vc=2.998e10  #speed of light
kb=1.38065e-16
rad2asec=180/np.pi*3600
asec2Mm = 0.73


hf=h5py.File(dem_file,'r')
logt_bin=np.array(hf['logt_bin'])
dem=np.array(hf['dem'])
hf.close()

T=10**logt_bin
N_temp_DEM=np.size(T)



GET_MW = initGET_MW(libname)

 
Lparms=np.zeros(5, dtype='int32') # array of dimensions etc.
Lparms[0]=NSteps
Lparms[1]=Nf
Lparms[2]=N_temp_DEM
Lparms[3]=0
Lparms[4]=1

 
Rparms=np.zeros(3, dtype='double') # array of global floating-point parameters - for a single LOS
Rparms[0]=(dx * asec2Mm * 1e8) * (dy * asec2Mm * 1e8) # area, cm^2
Rparms[1]=1e9  # starting frequency to calculate spectrum, Hz
Rparms[2]=0.1 # logarithmic step in frequency


depth_cm=1e10

ParmLocal = np.zeros(15, dtype='double')  # array of voxel parameters - for a single voxel
ParmLocal[0]=depth_cm/NSteps    #source depth, cm (total depth - the depths for individual voxels will be computed later)
ParmLocal[1]=1e6    #plasma temperature, K (not used in this example)
ParmLocal[2]=1e10    #electron/atomic concentration, cm^{-3} (not used in this example)
ParmLocal[3]=100  #magnetic field, G (will be changed later)
ParmLocal[4]=90  #viewing angle, degrees
ParmLocal[5]=0    #azimuthal angle, degrees
ParmLocal[6]=1+4      #emission mechanism flag (all on)
ParmLocal[7]=30     #maximum harmonic number
ParmLocal[8]=0      #proton concentration, cm^{-3} (not used in this example)
ParmLocal[9]=0      #neutral hydrogen concentration, cm^{-3}
ParmLocal[10]=0     #neutral helium concentration, cm^{-3}
ParmLocal[11]=0     #local DEM on/off key (on)
ParmLocal[12]=1    # local DDM on/off key (on)
ParmLocal[13]=0     #element abundance code (coronal, following Feldman 1992)
ParmLocal[14]=0     #reserved
 
Parms = np.zeros((15, NSteps), dtype='double', order='F')  # 2D array of input parameters - for multiple voxels
for i in range(NSteps):
    Parms[:, i] = ParmLocal  # most of the parameters are the same in all voxels
    



dem_shape=dem.shape

print (dem_shape)

Tbmap=np.zeros((dem_shape[0],dem_shape[1],Nf))



for i in range(dem_shape[0]):
	for j in range(dem_shape[1]):
		RL = np.zeros((7, Nf), dtype='double', order='F')  # input/output array
		DEM_arr=np.reshape(dem[i,j,:]/(depth_cm),(N_temp_DEM,NSteps))
		DDM_arr=DEM_arr
		res = GET_MW(Lparms, Rparms, Parms,T,DEM_arr,DDM_arr, RL)
		RR=RL[5]
		LL=RL[6]
		freqs=RL[0]
		Intensity=RR+LL
		sr = (dx / rad2asec) * (dy / rad2asec)
		Tb = Intensity * sfu2cgs * vc * vc / (2. * kb * (freqs * 1e9) * (freqs * 1e9) * sr)
		Tbmap[i, j, :] = Tb
		del DEM_arr, DDM_arr,RL




plt.imshow(Tbmap[:,:,0],origin='lower')
plt.show()

