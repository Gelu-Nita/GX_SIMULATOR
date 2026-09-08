;+
; Q-search panel legends: same strings as gx_processmodels_ebtel (a,b,
; PROJECTED/FINAL Q with neighbor-interval +/-, metric, tol, Run#).
; metric = 'res2' or 'chi2'. Draws two al_legend boxes (/top /left and
; /bottom /right).
;-
pro gx_chmp_qmetric_legend, ri, metric, charsize=charsize

  compile_opt idl2
  default, charsize, !p.charsize
  if ~isa(ri, 'STRUCT') then return
  which = strlowcase(strcompress(string(metric[0]), /rem))
  ab = string(ri.a, ri.b, format="('a=',f5.2,'; ','b=',f5.2)")
  if which eq 'chi2' then begin
    qb = ri.q_chi2_best
    qr = ri.q_chi2_range
    done = tag_exist(ri, 'chi2_done') ? keyword_set(ri.chi2_done) : 0
    qproj = string([qb, qr - qb], $
      format="('Q!Dchi2_best!N = ',g0,'!S!D',g0,'!R!U+',g0)")
    ystr = string(ri.chi2_best, format="('Chi!U2!N=',g0)")
  endif else begin
    qb = ri.q_res2_best
    qr = ri.q_res2_range
    done = tag_exist(ri, 'res2_done') ? keyword_set(ri.res2_done) : 0
    qproj = string([qb, qr - qb], $
      format="('Q!Dres2_best!N = ',g0,'!S!D',g0,'!R!U+',g0)")
    ystr = string(ri.res2_best, format="('RES!S!U2!N!R!Dnorm!N = ',g0)")
  endelse
  qfin = string([qb, qr - qb], format="('Q = ',g0,'!S!D',g0,'!R!U+',g0)")
  qleg = [ab, 'PROJECTED SOLUTION:', qproj]
  if done then qleg = [qleg, 'FINAL SOLUTION:', qfin]
  qleg = [qleg, ystr]
  gx_chmp_al_legend, qleg, /top, /left, charsize=charsize, box=1
  den = qr[0] + qr[1]
  tol = (finite(qr[0]) and finite(qr[1]) and (den ne 0)) ? $
    (qr[1] - qr[0]) / den : !values.d_nan
  rleg = [string(tol, format="('tol = ',g0)")]
  if tag_exist(ri, 'counter') then $
    rleg = [rleg, string(ri.counter, format="('Run#: ',g0)")]
  gx_chmp_al_legend, rleg, /bottom, /right, charsize=charsize, box=1
end
