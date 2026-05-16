; ==============================================================================
; Denial A-deck reader
; ------------------------------------------------------------------------------
; Load the denial-project A-deck file for one storm and parse the requested
; track information.
; ==============================================================================

pro get_adeck_denial,model,exper,stormname,atcfid,year,runtime,time,lat,lon,wind,pres,stat,fhr,verif

  print,'Checking for A deck file, downloading if needed'
  current_year=strmid(systime(0),3,4,/rev)
  yearmatch=(year eq current_year) ? 1 : 0

  data_dir=getenv('AO_DATA_DIR')
  if data_dir eq '' then data_dir='data'
  adeck_dir=data_dir+'/raw_denial/tracks/NHC_files/'+exper+'/'
  file_mkdir,adeck_dir,/noexpand_path
  ;if yearmatch then a_subdir='aid_public/a'+atcfid+'*'+year+'.dat.gz' else a_subdir='archive/'+year+'/a'+atcfid+'*'+year+'.dat.gz'
;  if yearmatch then a_subdir='aid_public/a'+atcfid+year+'.dat.gz' else a_subdir='archive/'+year+'/a'+atcfid+year+'.dat.gz'

  cd,adeck_dir
;  wget='wget -N -nH -nd -q https://ftp.nhc.noaa.gov/atcf/'+a_subdir
;  spawn,wget
;  if ~yearmatch then spawn,'gunzip -q a'+atcfid+'*'+year+'.dat.gz'
;  if  yearmatch then spawn,'gunzip -q a'+atcfid+'*'+year+'.dat.gz'
;  adeck_file='a'+atcfid+year+'.dat'
  atcfid2=strmid(atcfid,2,2)+strmid(atcfid,1,1)
  adeck_file=stormname+atcfid2+'.'+strmid(runtime,0,10)+'.trak.hwrf.atcfunix'

  ;create A-deck array for each selected storm and model
  a_maxlines=20000
  adeck=strarr(a_maxlines,9)
  grep='grep "'+model+'" '+adeck_file
  spawn,grep,tmp

  if strlen(tmp[0]) eq 0 then nr=0 else nr=n_elements(tmp)
  akeeper=0
  for rr=0,nr-1 do begin
    str=strsplit(tmp[rr],', ',/extract)
    if rr ge 1 then begin
      if strmid(tmp[rr],0,58) eq strmid(tmp[rr-1],0,58) then goto,a_extraline  ;character 58 is the end of the MSLP column
    endif
    adeck[akeeper,0]=str[2]+'00'                        ;id & init time (e.g. 201010291200)
    adeck[akeeper,1]=str[4]                             ;model (e.g. OFCL)
    adeck[akeeper,2]=str[5]                             ;forecast hour (e.g. 12)
    hemi=strmid(str[6],strlen(str[6])-1,1)
    if (hemi eq 'S') then begin
      str[6]=string(-float(strmid(str[6],0,strlen(str[6])-1))/10.)  ;124S -> -12.4
    endif else begin
      str[6]=string(float(strmid(str[6],0,strlen(str[6])-1))/10.)  ;124N -> 12.4
    endelse
    adeck[akeeper,3]=str[6]                             ;latitude (e.g. 268N)
    hemi=strmid(str[7],strlen(str[7])-1,1)
    if (hemi eq 'W') then begin
      str[7]=string(360.-float(strmid(str[7],0,strlen(str[7])-1))/10.) ;675W-> 292.5
    endif else begin
      str[7]=string(360.+float(strmid(str[7],0,strlen(str[7])-1))/10.) ;675E-> 67.5
    endelse
    adeck[akeeper,4]=str[7]                             ;longitude (e.g. 715W)
    adeck[akeeper,5]=str[8]                             ;intensity (e.g. 55)
    adeck[akeeper,6]=str[9]                             ;pressure (e.g. 981)
    adeck[akeeper,7]=str[10]                            ;status (e.g. HU)
    verfyyyy=strmid(str[2],0,4) & verfmm=strmid(str[2],4,2) & verfdd=strmid(str[2],6,2) & verfhh=strmid(str[2],8,2)
    verf_julian=julday(verfmm,verfdd,verfyyyy,double(verfhh))-2455000D +(str[5]/24.)
    caldat,verf_julian+2455000D,calmm,caldd,calyyyy,calhh
    verf_cal=string(calyyyy,format='(i4.4)')+string(calmm,format='(i2.2)')+string(caldd,format='(i2.2)')+string(calhh,format='(i2.2)')
    adeck[akeeper,8]=verf_cal+'00'   ;id & verif time (e.g. 201010291200)
    akeeper+=1
    a_extraline:
  endfor
  adeck=adeck[0:akeeper-1,*]
  nfcst=n_elements(uniq(adeck[*,0]))
  print,'A-Deck has '+strtrim(nfcst,2)+' unique forecast times'

  ;could have 11 times per forecast (0,3,12,24,36,48,72,96,120,144,168)
  time=adeck[*,0]
  fhr=adeck[*,2]
  lat= adeck[*,3]
  lon= adeck[*,4]
  wind=adeck[*,5]
  pres=adeck[*,6]
  stat=adeck[*,7]
  verif=adeck[*,8]

;  ;could have 11 times per forecast (0,3,12,24,36,48,72,96,120,144,168)
;  time=strarr(nfcst,11)
;  lat= fltarr(nfcst,11)
;  lon= fltarr(nfcst,11)
;  wind=fltarr(nfcst,11)
;  pres=fltarr(nfcst,11)
;  stat=strarr(nfcst,11)
;  verif=strarr(nfcst,11)
;stop

end
