; ==============================================================================
; Stormwindmodel input builder
; ------------------------------------------------------------------------------
; Create 4-column stormwindmodel input files from B-deck, A-deck, or Monte
; Carlo track sources.
; ==============================================================================

function swm_parse_manifest_fields, line, field_count

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

pro swm_load_storm_manifest, stormname, stormid, year, landfall, fcst, era5_start, era5_end, endfcst, found=found, manifest_path=manifest_path

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
    fields=swm_parse_manifest_fields(line,12L)
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

pro stormwindmodel_inputs

  ;=============================
  ; 1. Configure the run
  ;=============================

  stormname='milton'
  type='bdeck'  ;bdeck,adeck,mc
  ; If adeck, choose OFCL or OCD5. B-deck always maps to BEST.
  model='OFCL'

  ; In the denial project, A-deck files for ALL and NO use model name HWRF.
  denial=0  ;1 to use data denial files, 0 for regular A decks
  exper='NO'  ;ALL/NO
  if denial then model='HWRF'
  if type eq 'bdeck' then exper='BEST'

  ; Restrict B-deck times to the A-deck forecast windows, and vice versa.
  restrict_times=1

  tmp=getenv('AO_STORMNAME')
  if tmp ne '' then stormname=tmp
  tmp=getenv('AO_STORMWINDMODEL_TYPE')
  if tmp ne '' then type=tmp
  tmp=getenv('AO_STORMWINDMODEL_MODEL')
  if tmp ne '' then model=tmp
  tmp=getenv('AO_STORMWINDMODEL_DENIAL')
  if tmp ne '' then denial=fix(tmp)
  tmp=getenv('AO_DENIAL_EXPERIMENT')
  if tmp ne '' then exper=tmp
  tmp=getenv('AO_RESTRICT_TIMES')
  if tmp ne '' then restrict_times=fix(tmp)

  data_dir=getenv('AO_DATA_DIR')
  if data_dir eq '' then data_dir='data'
  mc_input_dir=getenv('AO_STORMWINDMODEL_MC_DIR')

  if denial then begin
    if mc_input_dir eq '' then mc_input_dir=data_dir+'/raw_denial/'+stormname+'/wind/inputs/'
    mcralldir=mc_input_dir
    outdir=data_dir+'/raw_denial/'+stormname+'/wind/inputs/'
  endif else begin
    if mc_input_dir eq '' then mc_input_dir=data_dir+'/raw/stormwindmodel_inputs/'+stormname+'/'
    mcralldir=mc_input_dir
    outdir=data_dir+'/raw/stormwindmodel_inputs/'+stormname+'/'    
  endelse

  file_mkdir,outdir,/noexpand_path

  manifest_path=getenv('AO_STORM_MANIFEST')
  manifest_stormid=''
  manifest_year=''
  manifest_landfall=''
  manifest_fcst=strarr(3)
  manifest_era5_start=''
  manifest_era5_end=''
  manifest_endfcst=strarr(3)
  swm_load_storm_manifest, stormname, manifest_stormid, manifest_year, manifest_landfall, manifest_fcst, manifest_era5_start, manifest_era5_end, manifest_endfcst, found=manifest_found, manifest_path=manifest_path

  if manifest_found then begin
    stormid=manifest_stormid
    year=manifest_year
    landfall=manifest_landfall
    ind=where(manifest_fcst ne '', n_fcst)
    if n_fcst gt 0 then fcst=manifest_fcst[ind]
  endif else begin
    case stormname of
    'allen': begin
      stormid='al04'
      year='1980'
      landfall='198008100600'
      fcst=['198008090600','198008050600','198008040600'] ;24h,48h,72h prior to landfall
    end
    'alicia': begin
      stormid='al03'
      year='1983'
      landfall='198308180700'
      fcst=['198308170600','198308160600'] ;24h,48h prior to landfall
    end
    'diana': begin
      stormid='al10'
      year='1984'
      landfall='198409130600'
      fcst=['198409120600','198409110600','198409100600'] ;24h,48h,72h prior to landfall
    end
    'bob85': begin
      stormid='al02'
      year='1985'
      landfall='198507250300'
      fcst=['198507240000','198507230000'] ;24h,48h prior to landfall
    end
    'danny85': begin
      stormid='al04'
      year='1985'
      landfall='198508151600'
      fcst=['198508141200'] ;24h prior to landfall
    end
    'elena': begin
      stormid='al05'
      year='1985'
      landfall='198509021300'
      fcst=['198509011200','198508311200','198508301200'] ;24h,48h,72h prior to landfall
    end
    'gloria': begin
      stormid='al09'
      year='1985'
      landfall='198509271600'
      fcst=['198509261200','198509251200','198509241200'] ;24h,48h,72h prior to landfall
    end
    'juan': begin
      stormid='al12'
      year='1985'
      landfall='198510291200'
      fcst=['198510281200','198510271200','198510261200'] ;24h,48h,72h prior to landfall
    end
    'kate': begin
      stormid='al13'
      year='1985'
      landfall='198511212200'
      fcst=['198511201800','198511191800','198511181800'] ;24h,48h,72h prior to landfall
    end
    'bonnie86': begin
      stormid='al02'
      year='1986'
      landfall='198606261000'
      fcst=['198606250600'] ;24h prior to landfall
    end
    'charley86': begin
      stormid='al05'
      year='1986'
      landfall='198608171400'
      fcst=['198608161200'] ;24h prior to landfall
    end
    'floyd87': begin
      stormid='al13'
      year='1987'
      landfall='198710121800'
      fcst=['198710111800','198710101800'] ;24h,48h prior to landfall
    end
    'florence88': begin
      stormid='al07'
      year='1988'
      landfall='198809100200'
      fcst=['198809090000','198809080000'] ;24h,48h prior to landfall
    end
    'chantal': begin
      stormid='al04'
      year='1989'
      landfall='198908011300'
      fcst=['198907311200'] ;24h prior to landfall
    end
    'hugo': begin
      stormid='al11'
      year='1989'
      landfall='198909220400'
      fcst=['198909210000','198909200000','198909190000'] ;24h,48h,72h prior to landfall
    end
    'jerry': begin
      stormid='al14'
      year='1989'
      landfall='198910160000'
      fcst=['198910150000','198910140000','198910130000'] ;24h,48h,72h prior to landfall
    end
    'bob': begin
      stormid='al03'
      year='1991'
      landfall='199108191800'
      fcst=['199108181800','199108171800','199108161800'] ;24h,48h,72h prior to landfall
    end
    'andrew': begin
      stormid='al04'
      year='1992'
      landfall='199208240800'
      fcst=['199208230600','199208220600','199208210600'] ;24h,48h,72h prior to landfall
    end
    'erin': begin
      stormid='al05'
      year='1995'
      landfall='199508031600'
      fcst=['199508021200','199508011200','199507311200'] ;24h,48h,72h prior to landfall
    end
    'opal': begin
      stormid='al17'
      year='1995'
      landfall='199510042200'
      fcst=['199510031800','199510021800','199510011800'] ;24h,48h,72h prior to landfall
    end
    'bertha': begin
      stormid='al02'
      year='1996'
      landfall='199607122000'
      fcst=['199607111800','199607101800','199607091800'] ;24h,48h,72h prior to landfall
    end
    'fran': begin
      stormid='al06'
      year='1996'
      landfall='199609060000'
      fcst=['199609050000','199609040000','199609030000'] ;24h,48h,72h prior to landfall
    end
    'hortense': begin
      stormid='al08'
      year='1996'
      landfall='199609100600'
      fcst=['199609090600','199609080600','199609070600'] ;24h,48h,72h prior to landfall
    end
    'danny': begin
      stormid='al05'
      year='1997'
      landfall='199707191200'
      fcst=['199707181200','199707171200'] ;24h,48h prior to landfall
    end
    'bonnie': begin
      stormid='al02'
      year='1998'
      landfall='199808270400'
      fcst=['199808260000','199808250000','199808240000'] ;24h,48h,72h prior to landfall
    end
    'earl': begin
      stormid='al05'
      year='1998'
      landfall='199809030600'
      fcst=['199809020600','199809010600'] ;24h,48h prior to landfall
    end
    'georges': begin
      stormid='al07'
      year='1998'
      landfall='199809281200'
      fcst=['199809271200','199809261200','199809251200'] ;24h,48h,72h prior to landfall
    end
    'bret': begin
      stormid='al03'
      year='1999'
      landfall='199908230000'
      fcst=['199908220000','199908210000','199908200000'] ;24h,48h,72h prior to landfall
    end
    'floyd': begin
      stormid='al08'
      year='1999'
      landfall='199909160600'
      fcst=['199909150600','199909140600','199909130600'] ;24h,48h,72h prior to landfall
    end
    'irene99': begin
      stormid='al13'
      year='1999'
      landfall='199910152000'
      fcst=['199910141800','199910131800','199910121800'] ;24h,48h,72h prior to landfall
    end
    'lili': begin
      stormid='al13'
      year='2002'
      landfall='200210031300'
      fcst=['200210021200','200210011200','200209301200'] ;24h,48h,72h prior to landfall
    end
    'claudette': begin
      stormid='al04'
      year='2003'
      landfall='200307151500'
      fcst=['200307141200','200307131200','200307121200'] ;24h,48h,72h prior to landfall
    end
    'isabel': begin
      stormid='al13'
      year='2003'
      landfall='200309181800'
      fcst=['200309171800','200309161800','200309151800'] ;24h,48h,72h prior to landfall
    end
    'charley': begin
      stormid='al03'
      year='2004'
      landfall='200408131900'
      fcst=['200408121800','200408111800','200408101800'] ;24h,48h,72h prior to landfall
    end
    'frances': begin
      stormid='al06'
      year='2004'
      landfall='200409050400'
      fcst=['200409040000','200409030000','200409020000'] ;24h,48h,72h prior to landfall
    end
    'gaston': begin
      stormid='al07'
      year='2004'
      landfall='200408291400'
      fcst=['200408281200'] ;24h prior to landfall
    end
    'ivan': begin
      stormid='al09'
      year='2004'
      landfall='200409160600'
      fcst=['200409150600','200409140600','200409130600'] ;24h,48h,72h prior to landfall
    end
    'jeanne': begin
      stormid='al11'
      year='2004'
      landfall='200409260400'
      fcst=['200409250000','200409240000','200409230000'] ;24h,48h,72h prior to landfall
    end
    'cindy': begin
      stormid='al03'
      year='2005'
      landfall='200507060300'
      fcst=['200507050000','200507040000'] ;24h,48h,72h prior to landfall
    end
    'dennis': begin
      stormid='al04'
      year='2005'
      landfall='200507101900'
      fcst=['200507091800','200507081800','200507071800'] ;24h,48h,72h prior to landfall
    end
    'katrina': begin
      stormid='al12'
      year='2005'
      landfall='200508291100'    
      fcst=['200508281200','200508271200','200508261200'] ;24h,48h,72h prior to landfall
    end
    'rita': begin
      stormid='al18'
      year='2005'
      landfall='200509240700'
      fcst=['200509230600','200509220600','200509210600'] ;24h,48h,72h prior to landfall
    end
    'wilma': begin
      stormid='al25'
      year='2005'
      landfall='200510241000'
      fcst=['200510230600','200510220600','200510210600'] ;24h,48h,72h prior to landfall
    end
    'dolly': begin
      stormid='al04'
      year='2008'
      landfall='200807231800'
      fcst=['200807221800','200807211800','200807201800'] ;24h,48h,72h prior to landfall
    end
    'gustav': begin
      stormid='al07'
      year='2008'
      landfall='200809011500'
      fcst=['200808311200','200808301200','200808291200'] ;24h,48h,72h prior to landfall
    end
    'ike': begin
      stormid='al09'
      year='2008'
      landfall='200809130700'
      fcst=['200809120600','200809110600','200809100600'] ;24h,48h,72h prior to landfall
    end
    'irene': begin
      stormid='al09'
      year='2011'
      landfall='201108271200'
      fcst=['201108261200','201108251200','201108241200'] ;24h,48h,72h prior to landfall
    end
    'isaac': begin
      stormid='al09'
      year='2012'
      landfall='201208290800'
      fcst=['201208281200','201208271200','201208261200'] ;24h,48h,72h prior to landfall
    end
    'sandy': begin
      stormid='al18'
      year='2012'
      landfall='201210292300'
      fcst=['201210290000','201210280000','201210270000'] ;24h,48h,72h prior to landfall
    end
    'arthur': begin
      stormid='al01'
      year='2014'
      landfall='201407040300'
      fcst=['201407030000','201407020000','201407010000'] ;24h,48h,72h prior to landfall
    end
    'hermine': begin
      stormid='al09'
      year='2016'
      landfall='201609020500'
      fcst=['201609010600','201608310600','201608300600'] ;24h,48h,72h prior to landfall
    end
    'matthew': begin
      stormid='al14'
      year='2016'
      landfall='201610081500'
      fcst=['201610071200','201610061200','201610051200'] ;24h,48h,72h prior to landfall
    end
    'harvey': begin
      stormid='al09'
      year='2017'
      landfall='201708260300'
      fcst=['201708250000','201708240000'] ;24h,48h,72h prior to landfall
    end
    'irma': begin
      stormid='al11'
      year='2017'
      landfall='201709101300'
      fcst=['201709091200','201709081200','201709071200'] ;24h,48h,72h prior to landfall
    end
    'maria': begin
      stormid='al15'
      year='2017'
      landfall='201709201000'
      fcst=['201709191200','201709181200','201709171200'] ;24h,48h,72h prior to landfall
    end
    'nate': begin
      stormid='al16'
      year='2017'
      landfall='201710080500'
      fcst=['201710070600','201710060600','201710050600'] ;24h,48h,72h prior to landfall
    end
    'florence': begin
      stormid='al06'
      year='2018'
      landfall='201809141100'
      fcst=['201809131200','201809121200','201809111200'] ;24h,48h,72h prior to landfall
    end
    'gordon': begin
      stormid='al07'
      year='2018'
      landfall='201809050300'
      fcst=['201809040000','201809030000'] ;24h,48h prior to landfall
    end
    'michael': begin
      stormid='al14'
      year='2018'
      landfall='201810101800'
      fcst=['201810091800','201810081800','201810071800'] ;24h,48h,72h prior to landfall
    end
    'barry': begin
      stormid='al02'
      year='2019'
      landfall='201907131500'
      fcst=['201907121200','201907111200'] ;24h,48h prior to landfall
    end
    'dorian': begin
      stormid='al05'
      year='2019'
      landfall='201909061200'
      fcst=['201909051200','201909041200','201909031200'] ;24h,48h,72h prior to landfall
    end
    'hanna': begin
      stormid='al08'
      year='2020'
      landfall='202007252200'
      fcst=['202007241800'] ;24h prior to landfall
    end
    'isaias': begin
      stormid='al09'
      year='2020'
      landfall='202008040300'
      fcst=['202008030000','202008020000','202008010000'] ;24h,48h,72h prior to landfall
    end
    'laura': begin
      stormid='al13'
      year='2020'
      landfall='202008270600'
      fcst=['202008260600','202008250600','202008240600'] ;24h,48h,72h prior to landfall
    end
    'sally': begin
      stormid='al19'
      year='2020'
      landfall='202009160900'
      fcst=['202009150600','202009140600','202009130600'] ;24h,48h,72h prior to landfall
    end
    'beta': begin
      stormid='al22'
      year='2020'
      landfall='202009220200'
      fcst=['202009210000','202009200000','202009190000'] ;24h,48h,72h prior to landfall
    end
    'delta': begin
      stormid='al26'
      year='2020'
      landfall='202010092300'
      fcst=['202010090000','202010080000','202010070000'] ;24h,48h,72h prior to landfall
    end
    'zeta': begin
      stormid='al28'
      year='2020'
      landfall='202010282100'
      fcst=['202010271800','202010261800','202010251800'] ;24h,48h,72h prior to landfall
    end
    'ida': begin
      stormid='al09'
      year='2021'
      landfall='202108291700'
      fcst=['202108281800','202108271800','202108261800'] ;24h,48h,72h prior to landfall
    end
    'nicholas': begin
      stormid='al14'
      year='2021'
      landfall='202109140600'
      fcst=['202109130600'] ;24h,48h,72h prior to landfall
    end
    'ian': begin
      stormid='al09'
      year='2022'
      landfall='202209281900'
      fcst=['202209271800','202209261800','202209251800'] ;24h,48h,72h prior to landfall
    end
    'nicole': begin
      stormid='al17'
      year='2022'
      landfall='202211100700'
      fcst=['202211090600','202211080600','202211070600'] ;24h,48h,72h prior to landfall
    end
    'idalia': begin
      stormid='al10'
      year='2023'
      landfall='202308301100'
      fcst=['202308291200','202308281200','202308271200'] ;24h,48h,72h prior to landfall
    end
    'beryl': begin
      stormid='al02'
      year='2024'
      landfall='202407080800'
      fcst=['202407070600','202407060600','202407050600'] ;24h,48h,72h prior to landfall
    end
    'debby': begin
      stormid='al04'
      year='2024'
      landfall='202408051100'
      fcst=['202408041200','202408031200','202408021200'] ;24h,48h,72h prior to landfall
    end
    'francine': begin
      stormid='al06'
      year='2024'
      landfall='202409112200'
      fcst=['202409110000','202409100000','202409090000'] ;24h,48h,72h prior to landfall
    end
    'helene': begin
      stormid='al09'
      year='2024'
      landfall='202409270300'
      fcst=['202409260000','202409250000','202409240000'] ;24h,48h,72h prior to landfall
    end
    'milton': begin
      stormid='al14'
      year='2024'
      landfall='202410100000'
      fcst=['202410090000','202410080000','202410070000'] ;24h,48h,72h prior to landfall
    end
    endcase
  endelse
  
  if type eq 'bdeck' then begin
    model='BEST'
    get_bdeck,model,stormid,year,valid_all,lat_all,lon_all,wind_all,pres_all,stat_all

    if restrict_times eq 1 then begin

      ;get the a-deck times so b-deck times can correspond to them
      for f=0,n_elements(fcst)-1 do begin
        if denial then begin
          get_adeck_denial,'HWRF','ALL',stormname,stormid,year,fcst[f],valid_tmp,lat_tmp,lon_tmp,wind_tmp,pres_tmp,stat_tmp,fhr_tmp,verif_tmp
        endif else begin
          get_adeck,'OFCL',stormid,year,valid_tmp,lat_tmp,lon_tmp,wind_tmp,pres_tmp,stat_tmp,fhr_tmp,verif_tmp          
        endelse

        ;need to grab just the one(s) you want before writing text file
        ;only get times at 00, 06, 12, 18Z
        ;later years include a 60h forecast, I'm not using that
;        ind=where(valid_tmp eq fcst[f] and fix(fhr_tmp) mod 6 eq 0 and fix(fhr_tmp) le 120 and fix(fhr_tmp) ne 60 and fix(fhr_tmp) ne 84 and fix(fhr_tmp) ne 108,cnt)
        ind=where(valid_tmp eq fcst[f] and fix(fhr_tmp) mod 6 eq 0 and fix(fhr_tmp) le 120,cnt)
        if cnt gt 0 then begin
          valid_tmp=verif_tmp[ind]
          lat_tmp=lat_tmp[ind]
          lon_tmp=lon_tmp[ind]
          wind_tmp=wind_tmp[ind]
          fhr_tmp=fhr_tmp[ind]
        endif else begin
          print,'No forecast made at this time, skipping.'
          stop;continue
        endelse

        ;this will allow all 6-hourly b-deck entries during the timeframe of the a-deck entries
;        starttime=valid_tmp[0]
;        endtime=valid_tmp[-1]
;        start_ind=where(valid_all eq starttime,cnt1)
;        end_ind=where(valid_all eq endtime,cnt2)
        match,valid_all,valid_tmp,bb,aa
        starttime=(valid_all[bb])[0]
        endtime=(valid_all[bb])[-1]
        start_ind=bb[0]
        end_ind=bb[-1]
        valid=valid_all[start_ind:end_ind]
        lat=lat_all[start_ind:end_ind]
        lon=lon_all[start_ind:end_ind]
        wind=wind_all[start_ind:end_ind]

;        ;this will only use times in the b-deck entries that correspond to times in the a-deck entry (not every 6 hours!)
;        match,valid_all,valid_tmp,inda,indb
;        valid=valid_all[inda]
;        lat=lat_all[inda]
;        lon=lon_all[inda]
;        wind=wind_all[inda]
;        fhr_tmp=fhr_tmp[0:n_elements(valid)-1]  ;if b-deck has fewer entries than a-deck, this will truncate it

        ;write out text file
        nlines=n_elements(wind)
        openw,lun,outdir+stormid+year+'_'+fcst[f]+'_'+type+'_'+model+'.csv',/get_lun
        for i=0,nlines-1 do begin
          printf,lun,valid[i]+','+string(lat[i],format='(f6.1)')+','+string(lon[i]-360.0,format='(f6.1)')+','+string(wind[i],format='(f6.1)')
        endfor
        close,lun & free_lun,lun

      endfor
    endif else begin
      valid=valid_all
      lat=lat_all
      lon=lon_all
      wind=wind_all
      ;;write out text file that includes the whole best-track
      ;nlines=n_elements(wind)
      ;openw,lun,outdir+stormid+year+'_'+type+'_'+valid[0]+'.csv',/get_lun
      ;for i=0,nlines-1 do begin
      ;  printf,lun,valid[i]+','+string(lat[i],format='(f6.1)')+','+string(lon[i]-360.0,format='(f6.1)')+','+string(wind[i],format='(f6.1)')
      ;endfor
      ;close,lun & free_lun,lun
      ;write out text file
      nlines=n_elements(wind)
;      openw,lun,outdir+stormid+year+'_'+valid[0]+'_'+type+'_'+model+'.csv',/get_lun
      openw,lun,outdir+stormid+year+'_'+valid[0]+'_'+type+'_'+exper+'.csv',/get_lun
      for i=0,nlines-1 do begin
        printf,lun,valid[i]+','+string(lat[i],format='(f6.1)')+','+string(lon[i]-360.0,format='(f6.1)')+','+string(wind[i],format='(f6.1)')
      endfor
      close,lun & free_lun,lun

    endelse
  endif


  if type eq 'adeck' then begin
    for f=0,n_elements(fcst)-1 do begin

      if denial then begin
        get_adeck_denial,'HWRF',exper,stormname,stormid,year,fcst[f],valid,lat,lon,wind,pres,stat,fhr,verif
      endif else begin
        get_adeck,model,stormid,year,valid,lat,lon,wind,pres,stat,fhr,verif        
      endelse

      get_bdeck,'BEST',stormid,year,valid_tmp,lat_tmp,lon_tmp,wind_tmp,pres_tmp,stat_tmp

      ;need to grab just the one(s) you want before writing text file
      ;only get times at 00, 06, 12, 18Z
      ind=where(valid eq fcst[f] and fix(fhr) mod 6 eq 0 and fix(fhr) le 120,cnt)

      if cnt gt 0 then begin
        valid=verif[ind]
        lat=lat[ind]
        lon=lon[ind]
        wind=wind[ind]
        fhr=fhr[ind]
      endif else begin
        print,'No forecast made at this time, skipping.'
        stop;continue
      endelse

;        ;this will allow all 6-hourly b-deck entries during the timeframe of the a-deck entries        
;        starttime=valid[0]
;        endtime=valid_tmp[-1]
;        start_ind=where(valid eq starttime,cnt1)
;        end_ind=where(valid eq endtime,cnt2)
        match,valid,valid_tmp,aa,bb
        starttime=(valid[aa])[0]
        endtime=(valid[aa])[-1]
        start_ind=aa[0]
        end_ind=aa[-1]
        valid=valid[start_ind:end_ind]
        lat=lat[start_ind:end_ind]
        lon=lon[start_ind:end_ind]
        wind=wind[start_ind:end_ind]

        ;write out text file
        nlines=n_elements(wind)
        if denial then begin
          openw,lun,outdir+stormid+year+'_'+valid[0]+'_'+type+'_'+exper+'.csv',/get_lun
        endif else begin
          openw,lun,outdir+stormid+year+'_'+valid[0]+'_'+type+'_'+model+'.csv',/get_lun          
        endelse

        for i=0,nlines-1 do begin
          printf,lun,valid[i]+','+string(lat[i],format='(f6.1)')+','+string(lon[i]-360.0,format='(f6.1)')+','+string(wind[i],format='(f6.1)')
        endfor
        close,lun & free_lun,lun

;      endif
    endfor
  endif


  if type eq 'mc' then begin
    get_bdeck,'BEST',stormid,year,valid_tmp,lat_tmp,lon_tmp,wind_tmp,pres_tmp,stat_tmp
    for f=0,n_elements(fcst)-1 do begin
      file='mcrall_'+stormid+year+'_'+strmid(fcst[f],4,6)+'.dat'
      if file_test(mcralldir+file) then begin
        header1=''
        header2=''
        data=''
        openr,lun,mcralldir+file,/get_lun
        ;first header
        readf,lun,header1
        ;second header
        readf,lun,header2
        tmp=strsplit(header2,' ',/extract)
        fhr1=float(tmp[0])
        fhr2=float(tmp[1])
        dt=float(tmp[2])
        nreal=float(tmp[3])
        nt=fhr2/dt+1
        ;NHC forecast
        for i=0,10 do begin
          readf,lun,data
        endfor
        ;forecasts from each realization
        rfhr=fltarr(nreal,nt)
        rint=fltarr(nreal,nt)
        rlat=fltarr(nreal,nt)
        rlon=fltarr(nreal,nt)
        rvalid=strarr(nreal,nt)
        for r=0,nreal-1 do begin
        ;for r=0,49 do begin
          for t=0,nt-1 do begin
            readf,lun,data
            tmp=strsplit(data,' ',/extract)
            rfhr[r,t]=double(tmp[2])
            rint[r,t]=float(tmp[3])
            rlat[r,t]=float(tmp[4])
            rlon[r,t]=float(tmp[5])

            verfyyyy=strmid(fcst[f],0,4) & verfmm=strmid(fcst[f],4,2) & verfdd=strmid(fcst[f],6,2) & verfhh=strmid(fcst[f],8,2)
            verf_julian=julday(verfmm,verfdd,verfyyyy,double(verfhh))-2455000D +(rfhr[r,t]/24D)
            caldat,verf_julian+2455000D,calmm,caldd,calyyyy,calhh
            verf_cal=string(calyyyy,format='(i4.4)')+string(calmm,format='(i2.2)')+string(caldd,format='(i2.2)')+string(calhh,format='(i2.2)')
            rvalid[r,t]=verf_cal+'00'

          endfor
          valid=rvalid[r,*]
          lat=rlat[r,*]
          lon=rlon[r,*]
          wind=rint[r,*]

          ;if the B-deck entries end before these do, truncate the output
          starttime=valid[0]
          endtime=valid_tmp[-1]
          start_ind=where(valid eq starttime,cnt1)
          end_ind=where(valid eq endtime,cnt2)
          valid=valid[start_ind:end_ind]
          lat=lat[start_ind:end_ind]
          lon=lon[start_ind:end_ind]
          wind=wind[start_ind:end_ind]


          ;write out text file
;          nlines=n_elements(rfhr[r,*])
          nlines=n_elements(valid)
          if denial then begin
            openw,lun2,outdir+stormid+year+'_'+type+string(r,format='(i4.4)')+'_'+valid[0]+'_'+exper+'.csv',/get_lun
          endif else begin
            openw,lun2,outdir+stormid+year+'_'+type+string(r,format='(i4.4)')+'_'+valid[0]+'.csv',/get_lun            
          endelse

          for i=0,nlines-1 do begin
            printf,lun2,valid[i]+','+string(lat[i],format='(f6.1)')+','+string(lon[i]-360.0,format='(f6.1)')+','+string(wind[i],format='(f6.1)')
          endfor
          close,lun2 & free_lun,lun2

        endfor
        close,lun & free_lun,lun
      endif

    endfor
  endif

end
