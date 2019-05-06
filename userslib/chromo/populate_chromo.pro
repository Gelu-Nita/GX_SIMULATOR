pro populate_chromo, model_mask,chromo
s=size(model_mask)
which,'gx_simulator',outfile=outfile,/quiet
path=file_dirname(file_dirname(outfile,/mark),/mark)+'userslib'+path_sep()+'chromo'+path_sep()


restore, path+'eduard_v3.sav'
temp_e=temp
nne_e=nne
np_e=np
nh_e=nh
nhi_e=nhi
dh_e=dh
h_e=h


restore, path+'fontenla_b_v3.sav'
temp_b=temp
nne_b=nne
np_b=np
nh_b=nh
nhi_b=nhi
dh_b=dh
h_b=h


restore, path+'fontenla_d_v3.sav'
temp_d=temp
nne_d=nne
np_d=np
nh_d=nh
nhi_d=nhi
dh_d=dh
h_d=h

restore, path+'fontenla_f_v3.sav'
temp_f=temp
nne_f=nne
np_f=np
nh_f=nh
nhi_f=nhi
dh_f=dh
h_f=h

restore, path+'fontenla_h_v3.sav'
temp_h=temp
nne_h=nne
np_h=np
nh_h=nh
nhi_h=nhi
dh_h=dh
h_h=h

restore, path+'fontenla_p_v3.sav'
temp_p=temp
nne_p=nne
np_p=np
nh_p=nh
nhi_p=nhi
dh_p=dh
h_p=h

restore, path+'fontenla_s_v3.sav'
temp_s=temp
nne_s=nne
np_s=np
nh_s=nh
nhi_s=nhi
dh_s=dh
h_s=h

restore, path+'fontenla_r_v3.sav'
temp_r=temp
nne_r=nne
np_r=np
nh_r=nh
nhi_r=nhi
dh_r=dh
h_r=h

temp=fltarr(s(1),s(2),n_elements(temp_b))
temp(*,*,*)=0
nne=fltarr(s(1),s(2),n_elements(temp_b))
nne(*,*,*)=0
np=fltarr(s(1),s(2),n_elements(temp_b))
np(*,*,*)=0
nh=fltarr(s(1),s(2),n_elements(temp_b))
nh(*,*,*)=0
nhi=fltarr(s(1),s(2),n_elements(temp_b))
nhi(*,*,*)=0
dh=fltarr(s(1),s(2),n_elements(temp_b))
dh(*,*,*)=0
h=fltarr(s(1),s(2),n_elements(temp_b))
h(*,*,*)=0


for i=0,s(1)-1 do $
 for j=0,s(2)-1 do $
   case model_mask(i,j) of
     1: begin
         temp(i,j,*)=temp_b(*) 
         nne(i,j,*)=nne_b(*) 
         np(i,j,*)=np_b(*) 
         nh(i,j,*)=nh_b(*) 
         nhi(i,j,*)=nhi_b(*)
         dh(i,j,*)=dh_b(*)
         h(i,j,*)=h_b(*)
         end
     2: begin
         temp(i,j,*)=temp_d(*) 
         nne(i,j,*)=nne_d(*) 
         np(i,j,*)=np_d(*) 
         nh(i,j,*)=nh_d(*) 
         nhi(i,j,*)=nhi_d(*)
         dh(i,j,*)=dh_d(*)
         h(i,j,*)=h_d(*)
        end 
     3: begin
         temp(i,j,*)=temp_f(*) 
         nne(i,j,*)=nne_f(*) 
         np(i,j,*)=np_f(*) 
         nh(i,j,*)=nh_f(*) 
         nhi(i,j,*)=nhi_f(*)
         dh(i,j,*)=dh_f(*)
         h(i,j,*)=h_f(*)
        end 
     4: begin
         temp(i,j,*)=temp_h(*) 
         nne(i,j,*)=nne_h(*) 
         np(i,j,*)=np_h(*) 
         nh(i,j,*)=nh_h(*) 
         nhi(i,j,*)=nhi_h(*)
         dh(i,j,*)=dh_h(*)
         h(i,j,*)=h_h(*)
        end 
     5: begin
         temp(i,j,*)=temp_p(*) 
         nne(i,j,*)=nne_p(*) 
         np(i,j,*)=np_p(*) 
         nh(i,j,*)=nh_p(*) 
         nhi(i,j,*)=nhi_p(*)
         dh(i,j,*)=dh_p(*)
         h(i,j,*)=h_p(*)
        end 
     6: begin
         temp(i,j,*)=temp_r(*) 
         nne(i,j,*)=nne_r(*) 
         np(i,j,*)=np_r(*) 
         nh(i,j,*)=nh_r(*) 
         nhi(i,j,*)=nhi_r(*)
         dh(i,j,*)=dh_r(*)
         h(i,j,*)=h_r(*)
        end 
     7: begin
         temp(i,j,*)=temp_s(*) 
         nne(i,j,*)=nne_s(*) 
         np(i,j,*)=np_s(*) 
         nh(i,j,*)=nh_s(*) 
         nhi(i,j,*)=nhi_s(*)
         dh(i,j,*)=dh_s(*)
         h(i,j,*)=h_s(*)
        end 
        
    10: begin
       temp(i,j,*)=temp_e(*)
       nne(i,j,*)=nne_e(*)
       np(i,j,*)=np_e(*)
       nh(i,j,*)=nh_e(*)
       nhi(i,j,*)=nhi_e(*)
       dh(i,j,*)=dh_e(*)
       h(i,j,*)=h_e(*)
     end
        
     else:   
    endcase 

dumm=replicate(0.0,s(1),s(2),n_elements(dh_b))
chromo={temp:dumm,nne:dumm,np:dumm,nh:dumm,nhi:dumm,dh:dumm,h:dumm} 
chromo.temp=temp
chromo.nne=nne
chromo.np=np
chromo.nh=nh
chromo.nhi=nhi
chromo.dh=dh
chromo.h=h
end








