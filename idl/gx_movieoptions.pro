function gx_movieoptions
  frm=['avi', 'flv', 'gif', 'mjpeg', 'mov', 'mp4', 'swf', 'wav', 'webm']
  formats=strcompress(frm[0],/rem)
  for i=1, n_elements(frm)-1 do formats=formats+'|'+strcompress(frm[i],/rem)
  desc = [ $
    '0, LABEL, Movie Output Options, CENTER', $
    '1, BASE,, ROW, FRAME', $
    '0, DROPLIST,'+ formats+', LABEL_TOP=Movie Format,Row, TAG=format', $
    '2, Float, 24, LABEL_TOP=Frames per second:, WIDTH=6, TAG=fps', $
    '1, BASE,, ROW', $
    '0, BUTTON, OK, QUIT,TAG=OK', $
    '2, BUTTON, Cancel, QUIT, TAG=CANCEL']
    opt=CW_FORM(desc,/Column,Title='Moview Options')
    ext=frm[opt.format]
    file=dialog_pickfile(filter='*.'+ext,$
                               DEFAULT_EXTENSION=ext,$
                               /write,/OVERWRITE_PROMPT,$
                               title='Please choose a filename to save this video') 
    return,file
end