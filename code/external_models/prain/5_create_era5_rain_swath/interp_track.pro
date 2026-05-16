; ==============================================================================
; Track interpolator
; ------------------------------------------------------------------------------
; Interpolate a storm track to a new time grid. DT, TIME, LAT, and LON are
; required inputs; all others are optional.
; ==============================================================================

pro interp_track,dt,time,btlat,btlon,$
  year=year,vmax=vmax,pres=pres,dist2land=dist2land,status=status,$
  categ=categ,lfflag=lfflag,r34=r34,qr34=qr34,rmw=rmw,spd=spd,dir=dir

  resolve_routine,'local_max_finder',/is_function
  resolve_routine,'atan2',/is_function

  ;check input time format (YYYYMMDDHHNN for B-deck, YYYY-MM-DD HH:NN for IBTRACS)
  if strmatch(time[0],'*:*') then begin
    y0=float(strmid(time,0,4))
    m0=float(strmid(time,5,2))
    d0=float(strmid(time,8,2))
    h0=float(strmid(time,11,2))
    n0=float(strmid(time,14,2))
    y1=float(strmid(time[0],0,4))
    m1=float(strmid(time[0],5,2))
    d1=float(strmid(time[0],8,2))
    h1=float(strmid(time[0],11,2))
    n1=float(strmid(time[0],14,2))
    y2=float(strmid(time[-1],0,4))
    m2=float(strmid(time[-1],5,2))
    d2=float(strmid(time[-1],8,2))
    h2=float(strmid(time[-1],11,2))
    n2=float(strmid(time[-1],14,2))
  endif else begin
    y0=float(strmid(time,0,4))
    m0=float(strmid(time,4,2))
    d0=float(strmid(time,6,2))
    h0=float(strmid(time,8,2))
    n0=float(strmid(time,10,2))
    y1=float(strmid(time[0],0,4))
    m1=float(strmid(time[0],4,2))
    d1=float(strmid(time[0],6,2))
    h1=float(strmid(time[0],8,2))
    n1=float(strmid(time[0],10,2))
    y2=float(strmid(time[-1],0,4))
    m2=float(strmid(time[-1],4,2))
    d2=float(strmid(time[-1],6,2))
    h2=float(strmid(time[-1],8,2))
    n2=float(strmid(time[-1],10,2))
  endelse

  ;create time_orig array
  time_orig=julday(m0,d0,y0,h0,n0)
  ;create time_interp array
  time_interp=timegen(start=julday(m1,d1,y1,h1,n1),final=julday(m2,d2,y2,h2,n2),step_size=dt,units='Minutes')

  ;format output time array
  caldat,time_interp,m3,d3,y3,h3,n3
  time_interp_str=strarr(n_elements(time_interp))
  for j=0L,n_elements(time_interp)-1 do begin
    time_interp_str[j]=string(y3[j],m3[j],d3[j],h3[j],n3[j],format='(i4.4,i2.2,i2.2,i2.2,i2.2)')
  endfor
  time=time_interp_str

  ;use spline interpolation for track
  if n_elements(time_orig) ge 4 then begin
    lon_interp =            interpol(btlon,time_orig,time_interp,/spline)
    lat_interp =            interpol(btlat,time_orig,time_interp,/spline)
  endif else begin
    lon_interp =            interpol(btlon,time_orig,time_interp)
    lat_interp =            interpol(btlat,time_orig,time_interp)
  endelse
  btlon=lon_interp
  btlat=lat_interp

  ;### OPTIONAL INPUTS/OUTPUTS ###
  ;use linear interpolation for everything else, then apply 3-hour smoothing to certain fields
  if dt le 180.0 then smoothing=180.0/dt else smoothing=1

    if n_elements(year) gt 0 then begin
      year_interp =           interpol(year,time_orig,time_interp) ;keep vmax values exactly the same as in best-track
      year=year_interp
    endif
    if n_elements(vmax) gt 0 then begin
      vmax_interp =           interpol(vmax,time_orig,time_interp) ;keep vmax values exactly the same as in best-track
      vmax=vmax_interp
    endif
    if n_elements(pres) gt 0 then begin
      pres_interp =           interpol(pres,time_orig,time_interp) ;keep pres values exactly the same as in best-track
      pres=pres_interp      
    endif
    if n_elements(status) gt 0 then begin
      status_interp =   round(interpol(status,time_orig,time_interp)) ;keep nature values exactly the same as in best-track
      status=status_interp      
    endif
    if n_elements(categ) gt 0 then begin
      categ_interp=lonarr(n_elements(time_interp))
      for j=0L,n_elements(time_interp)-1 do begin
        if round(vmax[j]) lt 34.                            then categ_interp[j]=0  ;TD
        if round(vmax[j]) ge 34.  and round(vmax[j]) lt 64.  then categ_interp[j]=1  ;TS
        if round(vmax[j]) ge 64.  and round(vmax[j]) lt 83.  then categ_interp[j]=2  ;H1
        if round(vmax[j]) ge 83.  and round(vmax[j]) lt 96.  then categ_interp[j]=3  ;H2
        if round(vmax[j]) ge 96.  and round(vmax[j]) lt 113. then categ_interp[j]=4  ;H3
        if round(vmax[j]) ge 113. and round(vmax[j]) lt 137. then categ_interp[j]=5  ;H4
        if round(vmax[j]) ge 137.                           then categ_interp[j]=6  ;H5
      endfor
      categ=categ_interp      
    endif
    if n_elements(lfflag) gt 0 then begin
      lfflag_interp =   fix(interpol(lfflag,time_orig,time_interp)+0.001) ;if interpol gives 0.999999, this will make it 1
      lfflag=lfflag_interp
    endif
    if n_elements(dist2land) gt 0 then begin
      dist2land_interp=smooth(interpol(dist2land,time_orig,time_interp),smoothing,/edge_truncate,/nan)
      dist2land=dist2land_interp
    endif
    if n_elements(r34) gt 0 then begin
      r34_interp =     smooth(interpol(r34,time_orig,time_interp),smoothing,/edge_truncate,/nan)
      r34=r34_interp      
    endif
    if n_elements(qr34) gt 0 then begin
      qr34_interp=dblarr(4,n_elements(time_interp))
      qr34_interp[0,*] = smooth(interpol(reform(qr34[0,*]),time_orig,time_interp),smoothing,/edge_truncate,/nan)
      qr34_interp[1,*] = smooth(interpol(qr34[1,*],time_orig,time_interp),smoothing,/edge_truncate,/nan)
      qr34_interp[2,*] = smooth(interpol(qr34[2,*],time_orig,time_interp),smoothing,/edge_truncate,/nan)
      qr34_interp[3,*] = smooth(interpol(qr34[3,*],time_orig,time_interp),smoothing,/edge_truncate,/nan)
      qr34=qr34_interp
    endif
    if n_elements(rmw) gt 0 then begin
      rmw_interp =     smooth(interpol(rmw,time_orig,time_interp),smoothing,/edge_truncate,/nan)
      rmw=rmw_interp      
    endif
    if n_elements(spd) gt 0 and n_elements(dir) gt 0 then begin
      ;wind direction cant be interpolated... switch to u/v then back to spd/dir later
      uu=spd*sin((270.0+180.0-dir)*!dtor)
      vv=spd*cos((270.0+180.0-dir)*!dtor)
      uu_interp =      smooth(interpol(uu,time_orig, time_interp),smoothing,/edge_truncate,/nan)
      vv_interp =      smooth(interpol(vv,time_orig, time_interp),smoothing,/edge_truncate,/nan)
      spd_interp = sqrt(uu_interp^2+vv_interp^2)
      dir_interp = atan2(vv_interp,uu_interp,/zero,/deg)
      spd=spd_interp
      dir=dir_interp      
    endif

end
