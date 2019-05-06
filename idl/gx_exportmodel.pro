function gx_exportmodel,model
if ~obj_valid(model) then model=gx_read()
if ~obj_isa(model,'gxmodel') then message,'No valid gxmodel provided'
b=model->GetB(bx=bx,by=by,bz=bz)
model->getproperty,xcoord_conv=dx,ycoord_conv=dy,zcoord_conv=dz,refmaps=refmaps,ew=ew,ns=ns
dr=[dx[1],dy[1],dz[1]]
box={bx:bx,by:by,bz:bz,dr:dr,refmaps:ptr_new(*refmaps)}
return,box
end