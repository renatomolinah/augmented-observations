; ==============================================================================
; B-deck reader
; ------------------------------------------------------------------------------
; Load or download the B-deck file for one storm and parse the requested track
; information.
; ==============================================================================

pro get_bdeck,model,atcfid,year,time,lat,lon,wind,pres,stat

  print,'Checking for B deck file, downloading if needed'
  current_year=strmid(systime(0),3,4,/rev)
  yearmatch=(year eq current_year) ? 1 : 0

  data_dir=getenv('AO_DATA_DIR')
  if data_dir eq '' then data_dir='data'
  bdeck_dir=getenv('AO_ATCF_DIR')
  if bdeck_dir eq '' then bdeck_dir=data_dir+'/raw/atcf/'
  skip_atcf_download=0
  tmp=getenv('AO_SKIP_ATCF_DOWNLOAD')
  if tmp ne '' then skip_atcf_download=fix(tmp)
  file_mkdir,bdeck_dir,/noexpand_path
  ;if yearmatch then b_subdir='btk/b'+atcfid+'*'+year+'.dat' else b_subdir='archive/'+year+'/b'+atcfid+'*'+year+'.dat.gz'
  if yearmatch then b_subdir='btk/b'+atcfid+year+'.dat' else b_subdir='archive/'+year+'/b'+atcfid+year+'.dat.gz'
  cd,bdeck_dir
  bdeck_file='b'+atcfid+year+'.dat'
  if ~file_test(bdeck_file) and ~skip_atcf_download then begin
    wget='wget -N -nH -nd -q https://ftp.nhc.noaa.gov/atcf/'+b_subdir
    spawn,wget
    if ~yearmatch then spawn,'gunzip -q b'+atcfid+'*'+year+'.dat.gz'
  endif
  if ~file_test(bdeck_file) then begin
    print,'Missing B-deck file: '+bdeck_dir+bdeck_file
    return
  endif

  ;create B-deck array for selected storm
  b_maxlines=1000
  bdeck=strarr(b_maxlines,8)
  date_cal_bdeck=strarr(b_maxlines)
  bkeeper=0

  grep='grep "'+model+'" '+bdeck_file
  spawn,grep,tmp
  nr=n_elements(tmp)
  for rr=0L,nr-1 do begin
    str=strsplit(tmp[rr],',',/extract)
    if rr ge 1 then begin
      ;        if strmid(tmp[rr],0,58) eq strmid(tmp[rr-1],0,58) then goto,b_extraline
      if strmid(tmp[rr],0,20) eq strmid(tmp[rr-1],0,20) then goto,b_extraline
    endif
    tmp1=strtrim(str[2],2)                      ;time (e.g. 2010102912)
    tmp2=strtrim(str[3],2)                      ;minutes (e.g. 30)
    if tmp2 eq '' then tmp2='00'
    bdeck[bkeeper,0]=tmp1+tmp2
    bdeck[bkeeper,1]=strtrim(str[4],2)                           ;model (e.g. BEST)
    bdeck[bkeeper,2]=strtrim(str[5],2)                           ;forecast hour (e.g. 0)
    hemi=strmid(str[6],strlen(str[6])-1,1)
    if (hemi eq 'S') then begin
      str[6]=string(-float(strmid(str[6],0,strlen(str[6])-1))/10.)  ;124S -> -12.4
    endif else begin
      str[6]=string(float(strmid(str[6],0,strlen(str[6])-1))/10.)  ;124N -> 12.4
    endelse
    bdeck[bkeeper,3]=str[6]                           ;latitude (e.g. 268N)
    hemi=strmid(str[7],strlen(str[7])-1,1)
    if (hemi eq 'W') then begin
      str[7]=string(360.-float(strmid(str[7],0,strlen(str[7])-1))/10.) ;675W-> 292.5
    endif else begin
      str[7]=string(float(strmid(str[7],0,strlen(str[7])-1))/10.) ;675E-> 67.5
    endelse
    bdeck[bkeeper,4]=strtrim(str[7],2)                           ;longitude (e.g. 715W)
    bdeck[bkeeper,5]=strtrim(str[8],2)                           ;intensity (e.g. 55)
    bdeck[bkeeper,6]=strtrim(str[9],2)                           ;pressure (e.g. 981)
    bdeck[bkeeper,7]=strtrim(str[10],2)                           ;status (e.g. TS)
    imm=strmid(bdeck[bkeeper,0],4,2) & idd=strmid(bdeck[bkeeper,0],6,2) & iyyyy=strmid(bdeck[bkeeper,0],0,4) & ihh=strmid(bdeck[bkeeper,0],8,2)
    date_julian=julday(imm,idd,iyyyy,double(ihh))-2455000D
    date_cal_bdeck[bkeeper]=date_julian
    ;date_cal_bdeck[bb,rr]=timegen(nt,units='Hours',step_size=6.0D,start=date_julian)
    bkeeper+=1
    b_extraline:
  endfor

  bdeck=bdeck[0:bkeeper-1,*]  ;this contains all unique lines in b-deck (regardless of storm status such as DB, LO, HU, etc)
  date_cal_bdeck=date_cal_bdeck[0:bkeeper-1,*]
  print,'B-Deck has '+strtrim(bkeeper,2)+' unique entries'

  time=strarr(bkeeper)
  lat= fltarr(bkeeper)
  lon= fltarr(bkeeper)
  wind=strarr(bkeeper)
  pres=fltarr(bkeeper)
  stat=strarr(bkeeper)
  for bb=0,bkeeper-1 do begin
    time[bb]=bdeck[bb,0]
    lat[bb]=bdeck[bb,3]
    lon[bb]=bdeck[bb,4]
    wind[bb]=bdeck[bb,5]
    pres[bb]=bdeck[bb,6]
    stat[bb]=bdeck[bb,7]
  endfor


end
