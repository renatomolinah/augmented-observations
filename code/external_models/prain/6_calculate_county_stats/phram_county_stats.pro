; ==============================================================================
; P-RAIN county statistics
; ------------------------------------------------------------------------------
; Combine P-RAIN ensemble output with observed ERA5 rainfall and aggregate both
; to the county level for forecast-error analysis.
; ==============================================================================

function phram_parse_manifest_fields, line, field_count

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

pro phram_load_storm_manifest, stormname, stormid, year, landfall, fcst, era5_start, era5_end, endfcst, found=found, manifest_path=manifest_path

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
    fields=phram_parse_manifest_fields(line,12L)
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

pro phram_county_stats

  resolve_all
  tic

  ;=============================
  ; 1. Configure the run
  ;=============================

  storm='milton' ;florence,michael
  exp=['24','48','72']

  denial=0
  ; exper='NO' ;If denial=1, set this to the appropriate experiment (e.g. ALL, NO).

  tmp=getenv('AO_STORMNAME')
  if tmp ne '' then storm=tmp
  tmp=getenv('AO_PHRAM_DENIAL')
  if tmp ne '' then denial=fix(tmp)
  tmp=getenv('AO_DENIAL_EXPERIMENT')
  if tmp ne '' then exper=tmp

  data_dir=getenv('AO_DATA_DIR')
  if data_dir eq '' then data_dir='data'
  idl_dir=getenv('AO_IDL_DIR')
  if idl_dir eq '' then begin
    if file_test('code/external_models/assets/prain/idl/us_county_stats.sav') then begin
      idl_dir='code/external_models/assets/prain/idl/'
    endif else begin
      idl_dir=data_dir+'/raw/prain/idl/'
    endelse
  endif
  inpath_init=getenv('AO_PHRAM_INPUT_DIR')
  outpath=getenv('AO_PHRAM_OUTPUT_DIR')
  if inpath_init eq '' then inpath_init=data_dir+'/raw/prain/phram/'+storm+'/'
  if outpath eq '' then outpath=data_dir+'/raw/precip_error/'
  if ~denial then exper=''

  if denial then begin
    if inpath_init eq '' or inpath_init eq data_dir+'/raw/prain/phram/'+storm+'/' then inpath_init=data_dir+'/raw_denial/'+storm+'/precip/phram/'+exper+'/'
    if outpath eq '' or outpath eq data_dir+'/raw/precip_error/' then outpath=data_dir+'/raw_denial/'+storm+'/precip/'    
  endif
  file_mkdir,outpath,/noexpand_path

  units='mm'  ;pick mm, cm, in

  manifest_path=getenv('AO_STORM_MANIFEST')
  manifest_stormid=''
  manifest_year=''
  manifest_landfall=''
  manifest_fcst=strarr(3)
  manifest_era5_start=''
  manifest_era5_end=''
  manifest_endfcst=strarr(3)
  phram_load_storm_manifest, storm, manifest_stormid, manifest_year, manifest_landfall, manifest_fcst, manifest_era5_start, manifest_era5_end, manifest_endfcst, found=manifest_found, manifest_path=manifest_path

  if manifest_found then begin
    atcf_id=manifest_stormid
    year=manifest_year
    ind=where(manifest_fcst ne '', n_runtime)
    if n_runtime gt 0 then runtime=manifest_fcst[ind]
  endif else begin
    ;DENNIS
    if storm eq 'cindy' then begin
    atcf_id='al03'
    year='2005'
    runtime=['200507050000','200507040000'] ;24h,48h prior to landfall
  endif
  if storm eq 'dennis' then begin
    atcf_id='al04'
    year='2005'
    runtime=['200507091800','200507081800','200507071800']
  endif
  if storm eq 'katrina' then begin
    atcf_id='al12'
    year='2005'
    runtime=['200508281200','200508271200','200508261200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'rita' then begin
    atcf_id='al18'
    year='2005'
    runtime=['200509230600','200509220600','200509210600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'wilma' then begin
    atcf_id='al25'
    year='2005'
    runtime=['200510230600','200510220600','200510210600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'dolly' then begin
    atcf_id='al04'
    year='2008'
    runtime=['200807221800','200807211800','200807201800'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'gustav' then begin
    atcf_id='al07'
    year='2008'
    runtime=['200808311200','200808301200','200808291200'] ;24h,48h,72h prior to landfall
  end
  if storm eq 'ike' then begin
    atcf_id='al09'
    year='2008'
    runtime=['200809120600','200809110600','200809100600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'irene' then begin
    atcf_id='al09'
    year='2011'
    runtime=['201108261200','201108251200','201108241200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'isaac' then begin
    atcf_id='al09'
    year='2012'
    runtime=['201208281200','201208271200','201208261200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'sandy' then begin
    atcf_id='al18'
    year='2012'
    runtime=['201210290000','201210280000','201210270000'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'arthur' then begin
    atcf_id='al01'
    year='2014'
    runtime=['201407030000','201407020000','201407010000'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'hermine' then begin
    atcf_id='al09'
    year='2016'
    runtime=['201609010600','201608310600','201608300600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'matthew' then begin
    atcf_id='al14'
    year='2016'
    runtime=['201610071200','201610061200','201610051200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'harvey' then begin
    atcf_id='al09'
    year='2017'
    runtime=['201708250000','201708240000'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'irma' then begin
    atcf_id='al11'
    year='2017'
    runtime=['201709091200','201709081200','201709071200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'maria' then begin
    atcf_id='al15'
    year='2017'
    runtime=['201709191200','201709181200','201709171200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'nate' then begin
    atcf_id='al16'
    year='2017'
    runtime=['201710070600','201710060600','201710050600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'florence' then begin
    atcf_id='al06'
    year='2018'
    runtime=['201809131200','201809121200','201809111200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'gordon' then begin
    atcf_id='al07'
    year='2018'
    runtime=['201809040000','201809030000'] ;24h,48h prior to landfall
  endif
  if storm eq 'michael' then begin
    atcf_id='al14'
    year='2018'
    runtime=['201810091800','201810081800','201810071800'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'barry' then begin
    atcf_id='al02'
    year='2019'
    runtime=['201907121200','201907111200'] ;24h,48h prior to landfall
  end
  if storm eq 'dorian' then begin
    atcf_id='al05'
    year='2019'
    runtime=['201909051200','201909041200','201909031200'] ;24h,48h,72h prior to landfall
  end
  if storm eq 'hanna' then begin
    atcf_id='al08'
    year='2020'
    runtime=['202007241800'] ;24h prior to landfall
  end
  if storm eq 'isaias' then begin
    atcf_id='al09'
    year='2020'
    runtime=['202008030000','202008020000','202008010000'] ;24h,48h,72h prior to landfall
  end
  if storm eq 'laura' then begin
    atcf_id='al13'
    year='2020'
    runtime=['202008260600','202008250600','202008240600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'sally' then begin
    atcf_id='al19'
    year='2020'
    runtime=['202009150600','202009140600','202009130600'] ;24h,48h,72h prior to landfall
  end
  if storm eq 'beta' then begin
    atcf_id='al22'
    year='2020'
    runtime=['202009210000','202009200000','202009190000'] ;24h,48h,72h prior to landfall
  end
  if storm eq 'delta' then begin
    atcf_id='al26'
    year='2020'
    runtime=['202010090000','202010080000','202010070000'] ;24h,48h,72h prior to landfall
  end
  if storm eq 'zeta' then begin
    atcf_id='al28'
    year='2020'
    runtime=['202010271800','202010261800','202010251800'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'ida' then begin
    atcf_id='al09'
    year='2021'
    runtime=['202108281800','202108271800','202108261800'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'nicholas' then begin
    atcf_id='al14'
    year='2021'
    runtime=['202109130600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'ian' then begin
    atcf_id='al09'
    year='2022'
    runtime=['202209271800','202209261800','202209251800'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'nicole' then begin
    atcf_id='al17'
    year='2022'
    runtime=['202211090600','202211080600','202211070600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'idalia' then begin
    atcf_id='al10'
    year='2023'
    runtime=['202308291200','202308281200','202308271200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'beryl' then begin
    atcf_id='al02'
    year='2024'
    runtime=['202407070600','202407060600','202407050600'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'debby' then begin
    atcf_id='al04'
    year='2024'
    runtime=['202408041200','202408031200','202408021200'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'francine' then begin
    atcf_id='al06'
    year='2024'
    runtime=['202409110000','202409100000','202409090000'] ;24h,48h,72h prior to landfall
  endif
  if storm eq 'helene' then begin
    atcf_id='al09'
    year='2024'
    runtime=['202409260000','202409250000','202409240000'] ;24h,48h,72h prior to landfall
  endif
    if storm eq 'milton' then begin
    atcf_id='al14'
    year='2024'
    runtime=['202410090000','202410080000','202410070000'] ;24h,48h,72h prior to landfall
    endif
  endelse

  ;these parameters are hardcoded in rainfall_topo.f, only change if that is changed
  nx=2401L     ;dimensions of rainfall grid
  ny=1801L     ;dimensions of rainfall grid
  delta=1/60.0 ;resolution of rainfall grid
  south=20.0
  north=50.0
  west=-105.0
  east=-65.0
  ;for Maria (Puerto Rico)
  if storm eq 'maria' then begin
    nx=1201L     ;dimensions of rainfall grid
    ny=1201L     ;dimensions of rainfall grid
    delta=1/60.0 ;resolution of rainfall grid
    south=10.0
    north=30.0
    west=-75.0
    east=-55.0
  endif

  lons=findgen(nx)*delta+west
  lats=findgen(ny)*delta+south
  dim=nx*ny
  phram_coords=fltarr(2,dim)
  kk=0L
  for jj=0,ny-1 do begin
    for ii=0,nx-1 do begin
      phram_coords[*,kk]=[lons[ii],lats[jj]]
      kk=kk+1
    endfor
  endfor

  case units of
    'mm': begin
      convert=1.0
      ;threshold=[20,80,160,320]
      threshold=[20,80,160]
      c_value_fill=findgen(81)*2.5
      c_value_line=[5,10,20,50,100,150,200,250,300,350,400,450,500]
    end
    'cm': begin
      convert=0.1
      ;threshold=[2,8,16,32]
      threshold=[2,8,16]
      c_value_fill=findgen(81)*0.25
      c_value_line=[0.5,1,2,5,10,15,20,25,30,35,40,45,50]
    end
    'in': begin
      convert=0.03937
      ;threshold=[1,3,6,12]
      ;threshold=[1,3,6]
      threshold=[3]
      c_value_fill=findgen(81)*0.1
      c_value_line=[0.1,0.2,0.5,1,2,4,6,8,10,12,14,16,18,20]
    end
  endcase

  ;read in US county data
  ;"county_data" includes state code, county code, county name, #parts, part#, center lon, center lat, $
  ;area (km2), number of vertices in boundary, xy coordinates of boundary vertices
  if storm ne 'maria' then begin
    print,'Using eastern CONUS counties'
    restore,idl_dir+'us_county_stats.sav',/relaxed_structure_assignment
  endif
  if storm eq 'maria' then begin
    print,'Using Puerto Rico counties'
    restore,idl_dir+'us_county_stats_puertorico.sav',/relaxed_structure_assignment    
  endif
  n_counties=county_data.Count()
  print,'Number of county parts: '+strtrim(n_counties)
  county_id=strarr(n_counties)
  for cc=0,n_counties-1 do begin
    county_id[cc]=(county_data[cc])[2]
  endfor
  ;get list of unique county FIPS codes
  county_id_uniq=county_id[uniq(county_id,sort(county_id))]
  n_counties_uniq=n_elements(county_id_uniq)
  print,'Number of unique counties: '+strtrim(n_counties)

  ;calculate centroid of entire county (not each part of a county)
  county_centerlon_uniq=dblarr(n_counties_uniq)
  county_centerlat_uniq=dblarr(n_counties_uniq)
  for cc=0,n_elements(county_id_uniq)-1 do begin
    vertices=[]
    ind=where(county_id eq county_id_uniq[cc],cnt)
    for i=0,cnt-1 do begin
      ii=ind[i]
      xy=(county_data[ii])[10]
      vertices=[[vertices],[xy]]      
    endfor
    o=obj_new('IDLanROI',vertices)        ;this object is the boundary coordinates of a county
    test=o->ComputeGeometry(centroid=centroid)
    county_centerlon_uniq[cc]=centroid[0]
    county_centerlat_uniq[cc]=centroid[1]
    obj_destroy,o
  endfor
  

;  county_list='/home/bmcnoldy/tmp/molina/stormwindmodel/county_list.csv'
;  n_counties=file_lines(county_list)-1
;  header=''
;  county_info=strarr(n_counties)
;  openr,lun,county_list,/get_lun
;  readf,lun,header
;  readf,lun,county_info
;  close,lun & free_lun,lun
;  fips_id=strarr(n_counties)
;  fips_county=strarr(n_counties)
;  fips_state=strarr(n_counties)
;  x_county_pts=fltarr(n_counties)
;  y_county_pts=fltarr(n_counties)
;  r_county=strarr(n_counties)
;  r_state=strarr(n_counties)
;  for c=0,n_counties-1 do begin
;    tmp=strsplit(county_info[c],',',/extract)
;    fips_id[c]=tmp[0]
;    fips_county[c]=tmp[1]
;    fips_state[c]=tmp[2]
;    x_county_pts[c]=double(tmp[3])
;    y_county_pts[c]=double(tmp[4])
;    r_county[c]=tmp[5]
;    r_state[c]=tmp[6]
;  endfor


  if ~denial then begin
    openw,1,width=150,outpath+atcf_id+year+'_'+storm+'_precip_errors.csv'
  endif else begin
    openw,1,width=150,outpath+atcf_id+year+'_'+storm+'_precip_errors_'+exper+'.csv'    
  endelse
  printf,1,'"","pred_date","lead_days","lead_hrs","gridid","glon","glat","prec_dif_mean","prec_dif_sd","prec_pred_mean","prec_pred_sd","prec_obs"'

  for t=0,n_elements(exp)-1 do begin

    exp_time=tic('exp'+exp[t])
    inpath=inpath_init+exp[t]+'/'
    files=file_search(inpath+'prain_????.txt.gz',count=zipped_members)
    if zipped_members gt 0 then begin
      print,'Unzipping '+strtrim(zipped_members,2)+' files in '+inpath+' ... this will take several (~20) minutes'
      print,systime()
      cmd='gunzip '+inpath+'prain_????.txt.gz'
      spawn,cmd
      print,systime()
    endif
    len=strlen(inpath)
    files=file_search(inpath+'prain_????.txt',count=nmembers)
    files=strmid(files,len)

    ;nmembers=50
    alldata=fltarr(nmembers,nx,ny)
    print,'Reading '+strtrim(nmembers,2)+' input files'
    for f=0,nmembers-1 do begin
      if f mod 100 eq 0 then print,f
      memberdata=fltarr(nx,ny) & memberdata[*,*]=0.0
      openr,2,inpath+files[f]
      readf,2,memberdata
      bad=where(memberdata lt 0,bad_cnt)
      if bad_cnt gt 0 then memberdata[bad]=0.0
      alldata[f,*,*]=memberdata*convert
      close,2 & free_lun,2
    endfor
    print,'Zipping '+strtrim(nmembers,2)+' files in '+inpath+' ... this will take several minutes (in background)'
    cmd='gzip -9 '+inpath+'prain_????.txt &'
    spawn,cmd,unit=unit

    ;calculate an ensemble mean and scale to the desired units
    print,'Calculating ensemble mean'
    ens_mean=mean(alldata,dim=1,/nan)
    ;calculate ensemble standard deviation
    print,'Calculating ensemble standard deviation'
    ens_stddev=stddev(alldata,dim=1,/nan)


    ;read in observed rainfall swath calculated from tc_rain_swath
    ;interpolate OBS RAIN grids to county points
    prec_indir=getenv('AO_PRAIN_ERA5_DIR')
    if prec_indir eq '' then prec_indir=data_dir+'/raw/era5/'
    prec_file=prec_indir+atcf_id+year+'_precip5day_'+runtime[t]+'.csv'
    openr,3,prec_file
    nlines=file_lines(prec_file)
    prec_lon=fltarr(nlines-1)
    prec_lat=fltarr(nlines-1)
    prec_swath=fltarr(nlines-1)
    line=''
    readf,3,line
    reads,line,prec_xdim,prec_ydim
    for pp=0L,nlines-2 do begin
      readf,3,line
      tmp=strsplit(line,',',/extract)
      prec_lon[pp]=tmp[0]-360.0
      prec_lat[pp]=tmp[1]
      prec_swath[pp]=tmp[2]
    endfor
    close,3 & free_lun,3
    prec_coords=transpose([[prec_lon],[prec_lat]])

    ;n_counties=n_elements(county_data)    
    ;center_lon=fltarr(n_counties)
    ;center_lat=fltarr(n_counties)
    ens_mean_county=fltarr(n_counties_uniq)   & ens_mean_county[*]=!values.f_nan
    ens_stddev_county=fltarr(n_counties_uniq) & ens_stddev_county[*]=!values.f_nan
    prec_obs_county=fltarr(n_counties_uniq)   & prec_obs_county[*]=!values.f_nan

    print,'Calculating county averages'
    for cc=0,n_counties_uniq-1 do begin
      if cc mod 400 eq 0 then print,cc,' of ',n_counties_uniq, '    ',systime()
      ;only calculate averages for counties in PHRAM domain
;      center_lon[cc]=(county_data[cc])[6]
;      center_lat[cc]=(county_data[cc])[7]
;      if center_lon[cc] ge west and center_lon[cc] le east and center_lat[cc] ge south and center_lat[cc] le north then begin
       if county_centerlon_uniq[cc] ge west and county_centerlon_uniq[cc] le east and county_centerlat_uniq[cc] ge south and county_centerlat_uniq[cc] le north then begin

        ;combine counties that have multiple parts
          in_phram=[]
          cnt_phram=0
          in_obs=[]
          cnt_obs=0
          ind=where(county_id eq county_id_uniq[cc],cnt)

          for i=0,cnt-1 do begin
            ii=ind[i]
            xy=(county_data[ii])[10]
            o=obj_new('IDLanROI',xy)

            ;average PHRAM data inside each county component
            test=o->ContainsPoints(phram_coords)
            in1=where(test gt 0,in_cnt1) ;0=exterior,1=inside bounds,2=on edge,3=on vertex
            in_phram=[in_phram,in1]
            cnt_phram=cnt_phram+in_cnt1
            
            ;average observed precip data inside each county component
            test=o->ContainsPoints(prec_coords)
            in2=where(test gt 0,in_cnt2) ;0=exterior,1=inside bounds,2=on edge,3=on vertex
            in_obs=[in_obs,in2]
            cnt_obs=cnt_obs+in_cnt2

            obj_destroy,o
          
          endfor

        if cnt_phram gt 0 then begin
          ens_mean_county[cc]=mean(ens_mean[in_phram],/nan)
          ens_stddev_county[cc]=mean(ens_stddev[in_phram],/nan)
        endif
        
        if cnt_obs gt 0 then begin
          prec_obs_county[cc]=mean(prec_swath[in_obs],/nan)
        endif

      endif else begin
        ;print,'Skipping: county '+strtrim(cc,2)+' of '+strtrim(n_counties_uniq,2)+' not in PHRAM domain'
      endelse
    endfor
  
;    set_plot,'x'
;    loadct,19,file='/home/bmcnoldy/idl/mycolors.tbl'
;    tvlct,r,g,b,/get
;    table=[[r],[g],[b]]
;    byte_ens_mean=bytscl(ens_mean_county,max=max(ens_mean_county),min=0.0,/nan)
;    byte_ens_stddev=bytscl(ens_stddev_county,max=max(ens_stddev_county),min=0.0,/nan)
;    byte_prec_obs=bytscl(prec_obs_county,max=max(prec_obs_county),min=0.0,/nan)
;    limit=[20,235,50,300]
;    map=map('Mollweide',center_longitude=270,limit=limit,position=[0.02,0.12,.98,.96],dimensions=[900,500])
;    scat=scatterplot(center_lon,center_lat,symbol='circle',/overplot,/sym_filled,sym_size=0.4,rgb_table=table,magnitude=byte_ens_mean)
;    map=map('Mollweide',center_longitude=270,limit=limit,position=[0.02,0.12,.98,.96],dimensions=[900,500])
;    scat=scatterplot(center_lon,center_lat,symbol='circle',/overplot,/sym_filled,sym_size=0.4,rgb_table=table,magnitude=byte_ens_stddev)
;    map=map('Mollweide',center_longitude=270,limit=limit,position=[0.02,0.12,.98,.96],dimensions=[900,500])
;    scat=scatterplot(center_lon,center_lat,symbol='circle',/overplot,/sym_filled,sym_size=0.4,rgb_table=table,magnitude=byte_prec_obs)
  
    diff_mean_county=ens_mean_county-prec_obs_county
    diff_stddev_county=ens_stddev_county-prec_obs_county

    diff_mean_county[where(~finite(diff_mean_county))]=-999.0
    diff_stddev_county[where(~finite(diff_stddev_county))]=-999.0
    ens_mean_county[where(~finite(ens_mean_county))]=-999.0
    ens_stddev_county[where(~finite(ens_stddev_county))]=-999.0
    prec_obs_county[where(~finite(prec_obs_county))]=-999.0
  
    for c=0,n_counties_uniq-1 do begin
      index='"'+strtrim(c,2)+'"'
      pred_date=strmid(runtime[t],0,4)+'-'+strmid(runtime[t],4,2)+'-'+strmid(runtime[t],6,2)+' '+strmid(runtime[t],8,2)+':00:00'
      lead_hours=strtrim(exp[t],2)
      lead_days=strtrim(fix(float(lead_hours)/24.0),2)
    
      printf,1,index,',',pred_date,',',lead_days,',',lead_hours,',',$
                 county_id_uniq[c],',',strtrim(county_centerlon_uniq[c],2),',',strtrim(county_centerlat_uniq[c],2),',',$
                 strtrim(diff_mean_county[c],2),',',strtrim(diff_stddev_county[c],2),',',$
                 strtrim(ens_mean_county[c],2),',',strtrim(ens_stddev_county[c],2),',',strtrim(prec_obs_county[c],2)
    endfor ;c
  
    toc,exp_time
  endfor ;t
  close,1 & free_lun,1

  toc
  ;stop

end
