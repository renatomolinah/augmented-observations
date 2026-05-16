; ==============================================================================
; ERA5 rainfall swath builder
; ------------------------------------------------------------------------------
; Aggregate hourly ERA5 precipitation into storm-level rainfall swaths over the
; P-RAIN domain used for the analysis.
; ==============================================================================

function era5_parse_manifest_fields, line, field_count

  fields=strarr(field_count)
  start=0L
  field_index=0L
  line_len=strlen(line)

  for ii=0L,line_len-1 do begin
    if strmid(line,ii,1) eq ',' then begin
      if field_index lt field_count then fields[field_index]=strmid(line,start,ii-start)
      start=ii+1L
      field_index=field_index+1L
    endif
  endfor

  if field_index lt field_count then fields[field_index]=strmid(line,start,line_len-start)

  return,fields
end

pro era5_load_storm_manifest, stormname, stormid, year, landfall, fcst, era5_start, era5_end, endfcst, found=found, manifest_path=manifest_path

  if n_elements(manifest_path) eq 0 then manifest_path=''
  if manifest_path eq '' then manifest_path='code/external_models/config/storm_manifest.csv'

  stormid=''
  year=''
  landfall=''
  fcst=strarr(3)
  era5_start=''
  era5_end=''
  endfcst=strarr(3)
  manifest_found=0

  if ~file_test(manifest_path) then begin
    found=manifest_found
    return
  endif

  nlines=file_lines(manifest_path)
  if nlines le 1 then begin
    found=manifest_found
    return
  endif

  openr,lun,manifest_path,/get_lun
  header=''
  readf,lun,header
  for line_index=0L,nlines-2 do begin
    line=''
    readf,lun,line
    fields=era5_parse_manifest_fields(line,12L)
    if fields[0] eq stormname then begin
      stormid=fields[1]
      year=fields[2]
      landfall=fields[3]
      fcst=[fields[4],fields[5],fields[6]]
      era5_start=fields[7]
      era5_end=fields[8]
      endfcst=[fields[9],fields[10],fields[11]]
      manifest_found=1
      break
    endif
  endfor
  close,lun & free_lun,lun

  found=manifest_found
end

pro tc_rain_swath_era5

  ;=============================
  ; 1. Configure the run
  ;=============================

  stormname='milton'
  restrict_times=1

  tmp=getenv('AO_STORMNAME')
  if tmp ne '' then stormname=tmp
  tmp=getenv('AO_RESTRICT_TIMES')
  if tmp ne '' then restrict_times=fix(tmp)

  ; ERA5 hourly precipitation files come from https://data.eol.ucar.edu/dataset/21.093.
  ; Filenames encode the ending time of the accumulation period.
  ; Units are mm on NCEP Grid 240 (HRAP), a 1121x881 polar-stereographic grid.
  data_dir=getenv('AO_DATA_DIR')
  if data_dir eq '' then data_dir='data'
  indir=getenv('AO_PRAIN_ERA5_DIR')
  if indir eq '' then indir=data_dir+'/raw/era5/'
  file_mkdir,indir,/noexpand_path
  wgrib=getenv('AO_WGRIB_EXE')
  if wgrib eq '' then wgrib='/usr/bin/wgrib'

  manifest_path=getenv('AO_STORM_MANIFEST')
  manifest_stormid=''
  manifest_year=''
  manifest_landfall=''
  manifest_fcst=strarr(3)
  manifest_era5_start=''
  manifest_era5_end=''
  manifest_endfcst=strarr(3)
  era5_load_storm_manifest, stormname, manifest_stormid, manifest_year, manifest_landfall, manifest_fcst, manifest_era5_start, manifest_era5_end, manifest_endfcst, found=manifest_found, manifest_path=manifest_path

  if manifest_found then begin
    stormid=manifest_stormid
    year=manifest_year
    landfall=manifest_landfall
    ind=where(manifest_fcst ne '', n_fcst)
    if n_fcst gt 0 then fcst=manifest_fcst[ind]
    ind_end=where(manifest_endfcst ne '', n_endfcst)
    if n_endfcst gt 0 then endfcst=manifest_endfcst[ind_end]
  endif else begin
    case stormname of
    'cindy': begin
      stormid='al03'
      year='2005'
      landfall='200507060300'
      fcst=['200507050000','200507040000'] ;24h,48h prior to landfall
      endfcst=['200507100000','200507090000'] ;add up to five days
    end
    'dennis': begin
      stormid='al04'
      year='2005'
      landfall='200507101900'
      fcst=['200507091800','200507081800','200507071800'] ;24h,48h,72h prior to landfall
      endfcst=['200507141800','200507131800','200507121800'] ;add up to five days
    end
    'katrina': begin
      stormid='al12'
      year='2005'
      landfall='200508291100'
      fcst=['200508281200','200508271200','200508261200'] ;24h,48h,72h prior to landfall
      endfcst=['200508310600','200508310600','200508310600'] ;add up to five days, 
      ;8/31 06Z is last, so these max out at 66h,90h,114h
    end
    'rita': begin
      stormid='al18'
      year='2005'
      landfall='200509240700'
      fcst=['200509230600','200509220600','200509210600'] ;24h,48h,72h prior to landfall
      endfcst=['200509260600','200509260600','200509260600'] ;add five days, 
      ;9/26 06Z is last, so these max out at 72h,96h,120h
    end
    'wilma': begin
      stormid='al25'
      year='2005'
      landfall='200510241000'
      fcst=['200510230600','200510220600','200510210600'] ;24h,48h,72h prior to landfall
      endfcst=['200510261800','200510261800','200510260600'] ;add five days, 
      ;10/26 18Z is last, so these max out at 84h,108h,120h
    end
    'dolly': begin
      stormid='al04'
      year='2008'
      landfall='200807231800'
      fcst=['200807221800','200807211800','200807201800'] ;24h,48h,72h prior to landfall
      endfcst=['200807270000','200807261800','200807251800'] ;add five days, 
      ;07/27 00Z is last, so these max out at 102h,120h,120h
    end
    'gustav': begin
      stormid='al07'
      year='2008'
      landfall='200809011500'
      fcst=['200808311200','200808301200','200808291200'] ;24h,48h,72h prior to landfall
      endfcst=['200809051200','200809041200','200809031200'] ;add five days, 
    end
    'ike': begin
      stormid='al09'
      year='2008'
      landfall='200809130700'
      fcst=['200809120600','200809110600','200809100600'] ;24h,48h,72h prior to landfall
      endfcst=['200809151200','200809151200','200809150600'] ;add five days  
      ;9/15 12Z is last, so these max out at 78h,102h,120h
    end
    'irene': begin
      stormid='al09'
      year='2011'
      landfall='201108271200'
      fcst=['201108261200','201108251200','201108241200'] ;24h,48h,72h prior to landfall
      endfcst=['201108300000','201108300000','201108291200'] ;add five days
      ;08/30 00Z is last, so these max out at 84h,108h,120h 
    end
    'isaac': begin
      stormid='al09'
      year='2012'
      landfall='201208290800'
      fcst=['201208281200','201208271200','201208261200'] ;24h,48h,72h prior to landfall
      endfcst=['201209010600','201209010600','201208311200'] ;add five days
      ;09/01 06Z is last, so these max out at 90h,114h,120h
    end
    'sandy': begin
      stormid='al18'
      year='2012'
      landfall='201210292300'
      fcst=['201210290000','201210280000','201210270000'] ;24h,48h,72h prior to landfall
      endfcst=['201210311200','201210311200','201210311200'] ;add five days
      ;10/31 12Z is last, so these max out at 60h,84h,108h
    end
    'arthur': begin
      stormid='al01'
      year='2014'
      landfall='201407040300'
      fcst=['201407030000','201407020000','201407010000'] ;24h,48h,72h prior to landfall
      endfcst=['201407080000','201407070000','201407060000'] ;add five days
    end
    'hermine': begin
      stormid='al09'
      year='2016'
      landfall='201609020500'
      fcst=['201609010600','201608310600','201608300600'] ;24h,48h,72h prior to landfall
      endfcst=['201609060600','201609050600','201609040600'] ;add five days
    end
    'matthew': begin
      stormid='al14'
      year='2016'
      landfall='201610081500'
      fcst=['201610071200','201610061200','201610051200'] ;24h,48h,72h prior to landfall
      endfcst=['201610100000','201610100000','201610100000'] ;add five days
      ;10/10 00Z is last so these max out at 60h,84h,108h
    end
    'harvey': begin
      stormid='al09'
      year='2017'
      landfall='201708260300'
      fcst=['201708250000','201708240000'] ;24h,48h,72h prior to landfall
      endfcst=['201708300000','201708290000'] ;add five days
    end
    'irma': begin
      stormid='al11'
      year='2017'
      landfall='201709101300'
      fcst=['201709091200','201709081200','201709071200'] ;24h,48h,72h prior to landfall
      endfcst=['201709131200','201709131200','201709121200'] ;add five days
      ;9/13 12Z is last, so these max out at 96h,120h,120h
    end
    'maria': begin
      stormid='al15'
      year='2017'
      landfall='201709201000'
      fcst=['201709191200','201709181200','201709171200'] ;24h,48h,72h prior to landfall
      endfcst=['201709241200','201709231200','201709221200'] ;add five days
    end
    'nate': begin
      stormid='al16'
      year='2017'
      landfall='201710080500'
      fcst=['201710070600','201710060600','201710050600'] ;24h,48h,72h prior to landfall
      endfcst=['201710110000','201710110000','201710100600'] ;add five days
      ;10/11 00Z is last, so these max out at 90h,114h,120h 
    end
    'florence': begin
      stormid='al06'
      year='2018'
      landfall='201809141100'
      fcst=['201809131200','201809121200','201809111200'] ;24h,48h,72h prior to landfall
      endfcst=['201809181200','201809171200','201809161200'] ;add five days
    end
    'gordon': begin
      stormid='al07'
      year='2018'
      landfall='201809050300'
      fcst=['201809040000','201809030000'] ;24h,48h prior to landfall
      endfcst=['201809071800','201809071800'] ;add five days
      ;09/07 18Z is last, so these max out at 90h,114h
    end
    'michael': begin
      stormid='al14'
      year='2018'
      landfall='201810101800'
      fcst=['201810091800','201810081800','201810071800'] ;24h,48h,72h prior to landfall
      endfcst=['201810141800','201810131800','201810121800'] ;add five days
    end
    'barry': begin
      stormid='al02'
      year='2019'
      landfall='201907131500'
      fcst=['201907121200','201907111200'] ;24h,48h prior to landfall
      endfcst=['201907160600','201907160600'] ;add five days
      ;7/16 06Z is last, so these max out at 90h,114h 
    end
    'dorian': begin
      stormid='al05'
      year='2019'
      landfall='201909061200'
      fcst=['201909051200','201909041200','201909031200'] ;24h,48h,72h prior to landfall
      endfcst=['201909090000','201909090000','201909081200'] ;add five days
      ;9/9 00Z is last, so these max out at 84h,108h,120h
    end
    'hanna': begin
      stormid='al08'
      year='2020'
      landfall='202007252200'
      fcst=['202007241800'] ;24h prior to landfall
      endfcst=['202007261800'] ;add five days
      ;7/26 18Z is last, so these max out at 48h
    end
    'isaias': begin
      stormid='al09'
      year='2020'
      landfall='202008040300'
      fcst=['202008030000','202008020000','202008010000'] ;24h,48h,72h prior to landfall
      endfcst=['202008050600','202008050600','202008050600'] ;add five days
      ;8/5 06Z is last, so these max out at 54h,78h,102h
    end
    'laura': begin
      stormid='al13'
      year='2020'
      landfall='202008270600'
      fcst=['202008260600','202008250600','202008240600'] ;24h,48h,72h prior to landfall
      endfcst=['202008290600','202008290600','202008290600'] ;add five days
      ;8/29 06Z is last, so these max out at 72h,96h,120h
    end
    'sally': begin
      stormid='al19'
      year='2020'
      landfall='202009160900'
      fcst=['202009150600','202009140600','202009130600'] ;24h,48h,72h prior to landfall
      endfcst=['202009180600','202009180600','202009180600'] ;add five days
      ;9/18 06Z is last, so these max out at 72h,96h,120h
    end
    'beta': begin
      stormid='al22'
      year='2020'
      landfall='202009220200'
      fcst=['202009210000','202009200000','202009190000'] ;24h,48h,72h prior to landfall
      endfcst=['202009250600','202009250000','202009240000'] ;add five days
      ;9/25 06Z is last, so these max out at 102h,120h,120h
    end
    'delta': begin
      stormid='al26'
      year='2020'
      landfall='202010092300'
      fcst=['202010090000','202010080000','202010070000'] ;24h,48h,72h prior to landfall
      endfcst=['202010111800','202010111800','202010111800'] ;add five days
      ;10/11 18Z is last, so these max out at 66h,90h,114h
    end
    'zeta': begin
      stormid='al28'
      year='2020'
      landfall='202010282100'
      fcst=['202010271800','202010261800','202010251800'] ;24h,48h,72h prior to landfall
      endfcst=['202010300000','202010300000','202010300000'] ;add five days
      ;10/30 00Z is last, so these max out at 54h,78h,102h
    end
    'ida': begin
      stormid='al09'
      year='2021'
      landfall='202108291700'
      fcst=['202108281800','202108271800','202108261800'] ;24h,48h,72h prior to landfall
      endfcst=['202109020600','202109011800','202108311800'] ;add five days
      ;9/2 06Z is last, so these max out at 108h,120h,120h
    end
    'nicholas': begin
      stormid='al14'
      year='2021'
      landfall='202109140600'
      fcst=['202109130600'] ;24h prior to landfall
      endfcst=['202109171800'] ;add five days
      ;9/17 18Z is last, so these max out at 108h
    end
    'ian': begin
      stormid='al09'
      year='2022'
      landfall='202209281900'
      fcst=['202209271800','202209261800','202209251800'] ;24h,48h,72h prior to landfall
      endfcst=['202210010600','202210010600','202209301800'] ;add five days
      ;10/01 06Z is last, so these max out at 84h,108h,120h 
    end
    'nicole': begin
      stormid='al17'
      year='2022'
      landfall='202211100700'
      fcst=['202211090600','202211080600','202211070600'] ;24h,48h,72h prior to landfall
      endfcst=['202211111800','202211111800','202211111800'] ;add five days
      ;11/11 18Z is last, so these max out at 60h,84h,108h
    end
    'idalia': begin
      stormid='al10'
      year='2023'
      landfall='202308301100'
      fcst=['202308291200','202308281200','202308271200'] ;24h,48h,72h prior to landfall
      endfcst=['202309031200','202309021200','202309011200'] ;add five days
    end
    'beryl': begin
      stormid='al02'
      year='2024'
      landfall='202407080800'
      fcst=['202407070600','202407060600','202407050600'] ;24h,48h,72h prior to landfall
      endfcst=['202407111200','202407110600','202407100600'] ;add five days
      ;7/11 12Z is last, so these max out at 102h,120h,120h
    end
    'debby': begin
      stormid='al04'
      year='2024'
      landfall='202408051100'
      fcst=['202408041200','202408031200','202408021200'] ;24h,48h,72h prior to landfall
      endfcst=['202408091200','202408081200','202408071200'] ;add five days
    end
    'francine': begin
      stormid='al06'
      year='2024'
      landfall='202409112200'
      fcst=['202409110000','202409100000','202409090000'] ;24h,48h,72h prior to landfall
      endfcst=['202409140000','202409140000','202409140000'] ;add five days
      ;9/14 00Z is last, so these max out at 72h,96,120h
    end
    'helene': begin
      stormid='al09'
      year='2024'
      landfall='202409270300'
      fcst=['202409260000','202409250000','202409240000'] ;24h,48h,72h prior to landfall
      endfcst=['202409281800','202409281800','202409281800'] ;add five days
      ;9/28 18Z is last, so these max out at 66h,90h,114h
    end
    'milton': begin
      stormid='al14'
      year='2024'
      landfall='202410100000'
      fcst=['202410090000','202410080000','202410070000'] ;24h,48h,72h prior to landfall
      endfcst=['202410111800','202410111800','202410111800'] ;add five days
      ;10/11 18Z is last, so these max out at 66h,90h,114h
    end
    endcase
  endelse


  ;get verification date/time from all available ERA5 files (unfortunately, filenames not very useful)
  era5_files=file_search(indir+'ecmf*.grib1')  
  awk="'"+'BEGIN{FS=":"};{print $3}'+"'"
  verif=strarr(n_elements(era5_files))
  for e=0,n_elements(era5_files)-1 do begin
    tmp=wgrib+' -verf -4yr -d 1 '+era5_files[e]+' | awk '+awk
    spawn,tmp,vrf
    verf=strsplit(vrf[0],'=',/extract) & verf=verf[1]
    verif[e]=verf
  endfor

  ;need to get lat/lon coordinates of precip grid, use first file
  ind=where(verif eq strmid(fcst[0],0,10),cnt)
  awk="'"+'BEGIN{RS="\n"};{print $2 " " $3 " " $4 " " $5 " " $6 " " $7 " " $10 " " $12}'+"'"
  tmp=wgrib+' -V -d 1 '+era5_files[ind]+' | awk '+awk
  spawn,tmp,grid
  n_n=strsplit(grid[2],' ',/extract)
  xdim=long(n_n[6]) & ydim=long(n_n[7])
  la_n=strsplit(grid[4],' ',/extract)
  lo_n=strsplit(grid[5],' ',/extract)
  prec_lats=(findgen(ydim)*float(la_n[5]))+float(la_n[3])
  prec_lons=(findgen(xdim)*float(lo_n[4]))+float(lo_n[0])+360.0


  ;read in B-deck
  model='BEST'
  get_bdeck,model,stormid,year,valid,lat,lon,wind,pres,stat


  ;loop over lead times
  for f=0,n_elements(fcst)-1 do begin

    if restrict_times eq 1 then begin
      starttime=strmid(fcst[f],0,4)+strmid(fcst[f],4,2)+strmid(fcst[f],6,2)+strmid(fcst[f],8,2)+'00'
      endtime=strmid(endfcst[f],0,4)+strmid(endfcst[f],4,2)+strmid(endfcst[f],6,2)+strmid(endfcst[f],8,2)+'00'
      start_ind=where(valid eq starttime,cnt1)
      end_ind=where(valid eq endtime,cnt2)
      if cnt1 eq 0 or cnt2 eq 0 then begin
        print,'Latest time in best-track file is '+valid[-1]
        stop  ;be careful entering the correct start and end times
      endif else begin
        time=valid[start_ind:end_ind]
        btlat=lat[start_ind:end_ind]
        btlon=lon[start_ind:end_ind]
      endelse
    endif

    ;interpolate data to higher time resolution (specify in minutes)
    dt=60.0
    interp_track,dt,time,btlat,btlon

    ;read in precip files
    prec_all=fltarr(n_elements(time)-1,xdim,ydim)
    prec_500=fltarr(n_elements(time)-1,xdim,ydim)
    xgrid=fltarr(xdim,ydim)
    ygrid=fltarr(xdim,ydim)
    rgrid=fltarr(xdim,ydim)
  
    for t=0,n_elements(time)-1 do begin
      ind=where(verif eq strmid(time[t],0,10),cnt)
      prec_fname=(era5_files[ind])[0]
      grib_id=grib_open(prec_fname)
      grib_handle=grib_new_from_file(grib_id)
      prec_orig=grib_get_values(grib_handle)
      prec_orig=reverse(prec_orig,2)
      prec_orig[where(prec_orig gt 5000.0)]=!values.f_nan  ;remove missing values (9999)

      hour=strmid(time[t],8,2)
      if t eq 0 then begin
        prec_prev=prec_orig       ;save the first time
        continue
      endif else if hour eq '01' then begin
        prec=prec_orig            ;the start of each day only contain rainfall from that one hour
      endif else begin
        prec=prec_orig-prec_prev  ;all other hours are accumulated throughout the day
      endelse

      ;create 500km circle around current storm center
      for xx=0L,xdim-1 do begin
        for yy=0L,ydim-1 do begin
          xgrid[xx,yy]=(prec_lons[xx]-btlon[t])*111.177*cos(prec_lats[yy]*!dtor)  ;was lons_p[xx]-lons_p[cxp]
          ygrid[xx,yy]=(prec_lats[yy]-btlat[t])*111.177                        ;was lats_p[yy]-lats_p[cyp]
          rgrid[xx,yy]=sqrt(xgrid[xx,yy]^2+ygrid[xx,yy]^2)
        endfor
      endfor

      prec_save=prec
      prec_save[where(rgrid gt 500.0)]=!values.f_nan    ;remove anything outside 500km from storm center
      
      ;save that hour to master grid
      prec_all[t-1,*,*]=prec
      prec_500[t-1,*,*]=prec_save
      prec_prev=prec_orig
    endfor ;t
    prec_all_swath=total(prec_all,1,/nan)*1000.0  ;convert to mm
    prec_500_swath=total(prec_500,1,/nan)*1000.0  ;convert to mm
    
    loadct,64,rgb_table=ct
    gridspacing=5
    m=map('Mercator',limit=[min(prec_lats),min(prec_lons),max(prec_lats),max(prec_lons)],dimensions=[900,800],position=[0.06,0.12,0.94,0.94],$
      title=strupcase(stormid)+year+' ('+strupcase(stormname)+'): '+fcst[f]+' - '+endfcst[f],$
      /box_axes,label_position=0,font_size=18,font_style='bold')
    ;c1=contour(prec_all_swath,prec_lons,prec_lats,grid_units='degrees',/fill,/overplot)
    c2=contour(prec_500_swath,prec_lons,prec_lats,c_value=(findgen(41)*5)+5,rgb_table=ct,grid_units='degrees',/fill,/overplot)
    mc=mapcontinents(/countries)
    mc=mapcontinents(/usa)
    grid=m.mapgrid
    grid.color='dark slate gray'
    grid.grid_longitude=gridspacing
    grid.grid_latitude=gridspacing
    grid.thick=0.1
    grid.font_size=9
    grid.clip=0
    grid.linestyle='dotted'
    m['Longitudes'].label_angle=0
    m['Latitudes'].label_angle=90
    p=plot(btlon,btlat,/overplot,color='red',thick=2)
    cb=colorbar(target=c2,title='mm',font_size=12,position=[0.2,0.06,0.8,0.08],/border,ticklen=0.1,subticklen=0)

    ;save text file with rain swath and coordinates
    openw,lun,indir+stormid+year+'_precip5day_'+fcst[f]+'.csv',/get_lun
    keep=intarr(xdim,ydim) & keep[*,*]=0
    xind=intarr(xdim,ydim)
    yind=intarr(xdim,ydim)

    phram_south=20
    phram_north=50
    phram_west=255
    phram_east=295
    if stormname eq 'maria' then begin
      phram_south=10
      phram_north=30
      phram_west=285
      phram_east=305      
    endif
    for yy=0,ydim-1 do begin
      for xx=0,xdim-1 do begin
        ;restrict output data to bounds I used for PHRAM output
        if prec_lons[xx] ge phram_west and prec_lons[xx] le phram_east and prec_lats[yy] ge phram_south and prec_lats[yy] le phram_north then begin
          keep[xx,yy]=1
          xind[xx,yy]=xx
          yind[xx,yy]=yy
;          printf,lun,string(prec_lons[xx,yy],format='(f10.6)'),',',string(prec_lats[xx,yy],format='(f10.6)'),',',string(prec_500_swath[xx,yy],format='(f10.6)')        
        endif
      endfor
    endfor
    xind_min=min(xind[where(keep eq 1)])
    xind_max=max(xind[where(keep eq 1)])
    yind_min=min(yind[where(keep eq 1)])
    yind_max=max(yind[where(keep eq 1)])
    xdim2=(xind_max-xind_min)+1
    ydim2=(yind_max-yind_min)+1
    print,xdim2,ydim2

    printf,lun,xdim2,ydim2
    for yy=yind_min,yind_max do begin
      for xx=xind_min,xind_max do begin
        ;restrict output data to bounds I used for PHRAM output
        printf,lun,string(prec_lons[xx],format='(f10.6)'),',',string(prec_lats[yy],format='(f10.6)'),',',string(prec_500_swath[xx,yy],format='(f10.3)')
      endfor
    endfor 
 
    close,lun & free_lun,lun
;stop
  endfor ;f

stop
end
