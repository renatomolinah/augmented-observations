#!/bin/bash
set -euo pipefail

################################################################################
# P-RAIN ensemble job
# ------------------------------------------------------------------------------
# Run one P-RAIN ensemble member for a selected forecast tag and gzip the
# resulting rainfall swath output.
################################################################################

imem=$(printf "%04d\n" "$2")
omem=$(printf "%04d\n" "$1")
echo ${omem}: using input member ${imem}

#
#=============================
# 1. Configure the run
#=============================

denial_exp="${AO_DENIAL_EXPERIMENT:-NO}"
stormname="${AO_STORMNAME:-milton}"
lead_hours="${AO_PRAIN_LEAD_HOURS:-72}"
data_dir="${AO_DATA_DIR:-data}"
asset_root="${AO_EXTERNAL_ASSET_ROOT:-$(CDPATH= cd -- "$(dirname -- "$0")"/../../.. && pwd)/code/external_models/assets}"

#.stag="al03_2005070400" #Cindy 48h 
#stag="al03_2005070500" #Cindy 24h 
#.stag="al04_2005070718" #Dennis 72h 
#.stag="al04_2005070818" #Dennis 48h
#.stag="al04_2005070918" #Dennis 24h
#.stag="al12_2005082612" #Katrina 72h
#.stag="al12_2005082712" #Katrina 48h
#.stag="al12_2005082812" #Katrina 24h
#.stag="al18_2005092106" #Rita 72h
#.stag="al18_2005092206" #Rita 48h
#.stag="al18_2005092306" #Rita 24h
#.stag="al25_2005102106" #Wilma 72h
#.stag="al25_2005102206" #Wilma 48h
#.stag="al25_2005102306" #Wilma 24h
#.stag="al04_2008072018" #Dolly 72h
#.stag="al04_2008072118" #Dolly 48h
#.stag="al04_2008072218" #Dolly 24h
#.stag="al07_2008082912" #Gustav 72h
#.stag="al07_2008083012" #Gustav 48h
#.stag="al07_2008083112" #Gustav 24h
#.stag="al09_2008091006" #Ike 72h
#.stag="al09_2008091106" #Ike 48h
#.stag="al09_2008091206" #Ike 24h
#.stag="al09_2011082412" #Irene 72h
#.stag="al09_2011082512" #Irene 48h
#.stag="al09_2011082612" #Irene 24h
#.stag="al09_2012082612" #Isaac 72h
#.stag="al09_2012082712" #Isaac 48h
#.stag="al09_2012082812" #Isaac 24h
#.stag="al18_2012102700" #Sandy 72h
#.stag="al18_2012102800" #Sandy 48h
#.stag="al18_2012102900" #Sandy 24h
#.stag="al01_2014070100" #Arthur 72h
#.stag="al01_2014070200" #Arthur 48h
#.stag="al01_2014070300" #Arthur 24h
#.stag="al09_2016083006" #Hermine 72h
#.stag="al09_2016083106" #Hermine 48h
#.stag="al09_2016090106" #Hermine 24h
#.stag="al14_2016100512" #Matthew 72h
#.stag="al14_2016100612" #Matthew 48h
#.stag="al14_2016100712" #Matthew 24h
#stag="al09_2017082400" #Harvey 48h
#stag="al09_2017082500" #Harvey 24h
#stag="al11_2017090712" #Irma 72h
#stag="al11_2017090812" #Irma 48h
#stag="al11_2017090912" #Irma 24h
#stag="al15_2017091712" #Maria 72h
#stag="al15_2017091812" #Maria 48h
#stag="al15_2017091912" #Maria 24h
#.stag="al16_2017100506" #Nate 72h
#.stag="al16_2017100606" #Nate 48h
#.stag="al16_2017100706" #Nate 24h
#stag="al06_2018091112" #Florence 72h
#stag="al06_2018091212" #Florence 48h
#stag="al06_2018091312" #Florence 24h
#stag="al07_2018090300" #Gordon 48h
#stag="al07_2018090400" #Gordon 24h
#stag="al14_2018100718" #Michael 72h
#stag="al14_2018100818" #Michael 48h
#stag="al14_2018100918" #Michael 24h
#.stag="al02_2019071112" #Barry 48h
#.stag="al02_2019071212" #Barry 24h
#stag="al05_2019090312" #Dorian 72h
#stag="al05_2019090412" #Dorian 48h
#stag="al05_2019090512" #Dorian 24h
#stag="al08_2020072418" #Hanna 24h
#stag="al09_2020080100" #Isaias 72h
#stag="al09_2020080200" #Isaias 48h
#stag="al09_2020080300" #Isaias 24h
#stag="al13_2020082406" #Laura 72h
#stag="al13_2020082506" #Laura 48h
#stag="al13_2020082606" #Laura 24h
#stag="al19_2020091306" #Sally 72h
#stag="al19_2020091406" #Sally 48h
#stag="al19_2020091506" #Sally 24h
#stag="al22_2020091900" #Beta 72h
#stag="al22_2020092000" #Beta 48h
#stag="al22_2020092100" #Beta 24h
#.stag="al26_2020100700" #Delta 72h
#.stag="al26_2020100800" #Delta 48h
#.stag="al26_2020100900" #Delta 24h
#.stag="al28_2020102518" #Zeta 72h
#.stag="al28_2020102618" #Zeta 48h
#.stag="al28_2020102718" #Zeta 24h
#.stag="al09_2021082618" #Ida 72h
#.stag="al09_2021082718" #Ida 48h
#.stag="al09_2021082818" #Ida 24h
#.stag="al14_2021091306" #Nicholas 24h
#stag="al09_2022092518" #Ian 72h
#stag="al09_2022092618" #Ian 48h
#stag="al09_2022092718" #Ian 24h
#stag="al17_2022110706" #Nicole 72h
#stag="al17_2022110806" #Nicole 48h
#stag="al17_2022110906" #Nicole 24h
#stag="al10_2023082912" #Idalia 24h
#stag="al10_2023082812" #Idalia 48h
#stag="al10_2023082712" #Idalia 72h
#stag="al02_2024070706" #Beryl 24h
#stag="al02_2024070606" #Beryl 48h
#stag="al02_2024070506" #Beryl 72h
#stag="al04_2024080412" #Debby 24h
#stag="al04_2024080312" #Debby 48h
#stag="al04_2024080212" #Debby 72h
#stag="al06_2024091100" #Francine 24h
#stag="al06_2024091000" #Francine 48h
#stag="al06_2024090900" #Francine 72h
#stag="al09_2024092600" #Helene 24h
#stag="al09_2024092500" #Helene 48h
#stag="al09_2024092400" #Helene 72h
#stag="al14_2024100900" #Milton 24h
#stag="al14_2024100800" #Milton 48h
stag="al14_2024100700" #Milton 72h
stag="${AO_PRAIN_STAG:-$stag}"


#=============================
# 2. Resolve paths
#=============================

whereami="${AO_PRAIN_HOME:-$PWD/}"
temp_root="${AO_PRAIN_TEMP_DIR:-${AO_PRAIN_SCRATCH:-${data_dir}/raw/prain/tmp/${stormname}/${lead_hours}/}}"
input_dir="${AO_PRAIN_INPUT_DIR:-${data_dir}/raw/prain/DATA/${stag}/}"
output_dir="${AO_PRAIN_OUTPUT_DIR:-${data_dir}/raw/prain/phram/${stormname}/${lead_hours}/}"

case "$whereami" in
  */) ;;
  *) whereami="${whereami}/" ;;
esac

case "$temp_root" in
  */) ;;
  *) temp_root="${temp_root}/" ;;
esac

case "$input_dir" in
  */) ;;
  *) input_dir="${input_dir}/" ;;
esac

case "$output_dir" in
  */) ;;
  *) output_dir="${output_dir}/" ;;
esac

ifile=${input_dir}storm_data_${imem}.txt
ofile=${output_dir}prain_${omem}.txt

default_topo="${asset_root}/prain/topo/US_topo_10_50N_50_130W.txt"
if [ ! -f "$default_topo" ]; then
  default_topo="${whereami}topo/US_topo_10_50N_50_130W.txt"
fi
topo="${AO_PRAIN_TOPO_FILE:-$default_topo}" #US_topo.txt
default_phram="${asset_root}/prain/bin/phram.exe"
if [ ! -x "$default_phram" ]; then
  default_phram="${whereami}create_prain/phram.exe"
fi
phram="${AO_PRAIN_EXE:-$default_phram}" #prain_2018AL06.exe  #${stag}.exe

tempdir=${temp_root}member_${omem}/
phram_in=${tempdir}storm_data.txt
phram_out=${tempdir}R-CLIPER.txt

#
#=============================
# 3. Run P-RAIN
#=============================

mkdir -p "$output_dir"
mkdir -p "$tempdir"
cp "$phram" "$tempdir/phram.exe"
cp "$ifile" "${phram_in}"
cp "$topo" "${tempdir}/US_topo.txt"
cd "$tempdir"
./phram.exe
cp "${phram_out}" "${ofile}"
cd "${whereami}"
rm -r "$tempdir"
gzip -9 "${ofile}"

exit 0
