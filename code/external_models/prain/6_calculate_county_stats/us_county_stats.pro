; ==============================================================================
; County statistics builder
; ------------------------------------------------------------------------------
; Build county lookup objects used by the rainfall and wind post-processing
; workflows, including a Puerto Rico export for stormwindmodel.
; ==============================================================================

pro us_county_stats

  data_dir=getenv('AO_DATA_DIR')
  if data_dir eq '' then data_dir='data'
  output_idl_dir=getenv('AO_IDL_DIR')
  if output_idl_dir eq '' then begin
    if file_test('code/external_models/assets/prain/idl/us_county_stats.sav') then begin
      output_idl_dir='code/external_models/assets/prain/idl/'
    endif else begin
      output_idl_dir=data_dir+'/raw/prain/idl/'
    endelse
  endif
  file_mkdir,output_idl_dir,/noexpand_path

  if !version.os_family eq 'unix' then begin
    idl_dir=output_idl_dir
    gis_dir=getenv('AO_GIS_DIR')
    if gis_dir eq '' then gis_dir='GISfiles/'
  endif
  if !version.os_family eq 'Windows' then begin
    idl_dir=output_idl_dir
    gis_dir=getenv('AO_GIS_DIR')
    if gis_dir eq '' then gis_dir='GISfiles\'
  endif

  domain='florida'  ;us, puertorico, florida
  tmp=getenv('AO_COUNTY_DOMAIN')
  if tmp ne '' then domain=tmp
  

  ;US county shapefile
  shpfile=gis_dir+'cb_2018_us_county_20m'
  shp=obj_new('IDLffShape',shpfile)
  shp->GetProperty,n_entities=n_ent,n_attributes=n_att,attribute_names=att_names,attribute_info=att_info
  county_data=list()

  ;create Puerto Rico text file formatted for stormwindmodel in R?
  if domain eq 'puertorico' then begin
    openw,lun,idl_dir+'us_county_stats_puertorico.csv',/get_lun
    printf,lun,'"gridid","glat","glon"'
  endif

  for e=0,n_ent-1 do begin
;  for e=0,16 do begin
    area=0.0D
    attr=shp->GetAttributes(e)
    ent=shp->GetEntity(e)
    statename=attr.ATTRIBUTE_0
    countycode=attr.ATTRIBUTE_4
    countyname=attr.ATTRIBUTE_5
    aland=attr.ATTRIBUTE_7
    aland=aland/1.0e6
    nparts=ent.N_PARTS
    parts=*ent.parts
    nvert=ent.N_VERTICES
    xvert=reform((*ent.vertices)[0,*])
    yvert=reform((*ent.vertices)[1,*])

if countycode eq '12086' then begin
    m=map('Cylindrical Equal Area',center_longitude=298.257,semimajor_axis=6378137.0,semiminor_axis=6356752.31414,ellipsoid='GRS1980',/buffer)
    ;those values in the map projection came from the .PRJ file
    xy=m.mapforward(xvert,yvert)
    xvert_cart=reform(xy[0,*])
    yvert_cart=reform(xy[1,*])

    for p=0L,nparts-1 do begin
      if nparts gt 1 then begin           ;does the entity have more than one component?
        if p lt nparts-1 then begin
          xx_cart=xvert_cart[parts[p]:parts[p+1]-1]
          yy_cart=yvert_cart[parts[p]:parts[p+1]-1]
          xx=xvert[parts[p]:parts[p+1]-1]
          yy=yvert[parts[p]:parts[p+1]-1]
        endif else begin
          xx_cart=xvert_cart[parts[p]:-1]
          yy_cart=yvert_cart[parts[p]:-1]            
          xx=xvert[parts[p]:-1]
          yy=yvert[parts[p]:-1]
        endelse
      endif else begin
        xx_cart=xvert_cart
        yy_cart=yvert_cart
        xx=xvert
        yy=yvert
      endelse
      area_part=poly_area(xx_cart,yy_cart,/double)/1.0e6  ;km^2
      area=area+area_part
      
      print,e,',',countyname,', ',statename,', ',strtrim(area_part,2),' km2'
;      print,e,',',countyname,', ',statename,', ',strtrim(aland,2),' km2'

      if area_part gt 10 then begin
        xy=transpose([[xx],[yy]])
        o=obj_new('IDLanROI',xy)        ;this object is the boundary coordinates of a county
        test=o->ComputeGeometry(centroid=centroid)
        county_centerlon=centroid[0]
        county_centerlat=centroid[1]
        obj_destroy,o

;        in=where(test gt 0,in_cnt) ;0=exterior,1=inside bounds,2=on edge,3=on vertex

;        pl=plot(lons,lats,/nodata)
;        if in_cnt gt 0 then begin
;          flagstate[e]=1
;          pl2=plot(xx,yy,/overplot,color='black')
;          for i=0,in_cnt-1 do begin
;            s1=symbol(coords[0,in[i]],coords[1,in[i]],'circle',sym_color='red',/data)
;          endfor
;          stop
;        endif
;stop
        tmp=list('Data',statename,countycode,countyname,nparts,parts,county_centerlon,county_centerlat,area_part,nvert,xy)

        case domain of
          'puertorico': begin
            if strmid(countycode,0,2) eq '72' then begin
              ;omit uninhabited Isla de Mona, second part of Mayaguez County, ID 72097, 18.089310,-67.888765
              if countycode eq '72097' and county_centerlon lt -67.5 then break
              county_data.add,tmp
              ;create a simple text file for Puerto Rico formatted for stormwindmodel in R
              printf,lun,'"',countycode,'"',',',string(county_centerlat,format='(f9.6)'),',',string(county_centerlon,format='(f10.6)')
            endif          
          end
          'florida': begin
            if strmid(countycode,0,2) eq '12' then begin
              county_data.add,tmp
            endif
          end
          else: begin
            county_data.add,tmp
          end          
        endcase

;        if ~puertorico then begin
;          county_data.add,tmp
;        endif
;        if puertorico then begin
;          if strmid(countycode,0,2) eq '72' then begin
;
;            ;omit uninhabited Isla de Mona, second part of Mayaguez County, ID 72097, 18.089310,-67.888765
;            if countycode eq '72097' and county_centerlon lt -67.5 then continue
;
;            county_data.add,tmp                   
;
;            ;create a simple text file for Puerto Rico formatted for stormwindmodel in R
;            printf,lun,'"',countycode,'"',',',string(county_centerlat,format='(f9.6)'),',',string(county_centerlon,format='(f10.6)')          
;          endif
;        endif
;        if florida then begin
;          if strmid(countycode,0,2) eq '12' then begin
;
;            county_data.add,tmp
;
;            ;create a simple text file for Puerto Rico formatted for stormwindmodel in R
;            printf,lun,'"',countycode,'"',',',string(county_centerlat,format='(f9.6)'),',',string(county_centerlon,format='(f10.6)')
;          endif
;
;
      endif
    endfor ;p
    m.close  ;clears graphics buffer

endif

  endfor ;e

  ;close Puerto Rico text file
  if domain eq 'puertorico' then begin
    close,lun & free_lun,lun
  endif


  obj_destroy,shp

  case domain of
    'puertorico': save,county_data,filename=idl_dir+'us_county_stats_puertorico.sav'
    'florida': save,county_data,filename=idl_dir+'us_county_stats_florida.sav'
    else: save,county_data,filename=idl_dir+'us_county_stats.sav'
  endcase 
  ;if ~puertorico then save,county_data,filename=idl_dir+'us_county_stats.sav'
  ;if  puertorico then save,county_data,filename=idl_dir+'us_county_stats_puertorico.sav'
  ;if  florida    then save,county_data,filename=idl_dir+'us_county_stats_florida.sav'

stop
end
