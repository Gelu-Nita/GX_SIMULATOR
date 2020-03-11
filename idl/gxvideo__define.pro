
function gxVIDEO::Init,dimensions, stream=stream,title=title,artist=artist,fps=fps,_extra=_extra
  compile_opt hidden
  catch, error_stat
  if error_stat ne 0 then begin
    catch, /cancel
    MESSAGE, /INFO, !ERROR_STATE.MSG
    return, obj_new()
  end
  frm=['mp4', 'avi', 'flv', 'gif', 'mjpeg', 'mov', 'swf', 'wav', 'webm']
  formats=strcompress(frm[0],/rem)
  for i=1, n_elements(frm)-1 do formats=formats+'|'+strcompress(frm[i],/rem)
  default,fps,24
  desc = [ $
    '0, LABEL, Movie Output Options, CENTER', $
    '1, BASE,, ROW, FRAME', $
    '0, DROPLIST,'+ formats+', LABEL_TOP=Movie Format,Row, TAG=format', $
    '2, Float,'+string(fps,format='(i2)')+', LABEL_TOP=Frames per second:, WIDTH=6, TAG=fps', $
    '1, BASE,, ROW', $
    '0, BUTTON, OK, QUIT,TAG=OK', $
    '2, BUTTON, Cancel, QUIT, TAG=CANCEL']
  opt=CW_FORM(desc,/Column,Title='Moview Options')
  if opt.OK ne 1 then return, !null
  ext=frm[opt.format]
  filename=dialog_pickfile(filter='*.'+ext,$
    DEFAULT_EXTENSION=ext,$
    /write,/OVERWRITE_PROMPT,$
    title='Please choose a filename to save this video')
   fps=opt.fps
   result=self->IDLffVideoWrite::Init(Filename,_extra=_extra)
   if result eq 1 then begin
    default,title,'GX _Simulator Video'
    default,artist,'GX_Simulator'
    self.SetMetadata,'title',title
    self.SetMetadata,'artist',artist
    stream = self.AddVideoStream(dimensions[0], dimensions[1], fps)
   endif
   return,self
end

pro gxVideo__define
 struct_hide, {gxVideo, inherits IDLffVideoWrite}
end