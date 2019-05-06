;utility to populate a gxm model with bmed and length for each voxel crossed by a closed field line
pro gx_ccm,file
 model=gx_read(file)
 model->ComputeCoronalModel,/ABOVE_TR,/Compute
 save,model,file=file
end