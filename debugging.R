PalFun<-function(PalName,N,Names) {
  Viridis<-data.table(Source="viridis",Palette=c("magma","inferno","plasma","viridis","cividis","rocket","mako","turbo"))
  Met<-data.table(Source="MetBrewer",Palette=names(MetBrewer::MetPalettes))
  Palettes<-rbind(Viridis,Met)
  
  if(Palettes[Palette==PalName,Source]=="viridis"){
    PAL<-viridis::viridis(N,option=PalName)
  }
  
  if(Palettes[Palette==PalName,Source]=="MetBrewer"){
    PAL<-MetBrewer::met.brewer(name=PalName, n=N, type="continuous")
  }
  
  if(Palettes[Palette==PalName,Source]=="Wes"){
    PAL<-wesanderson::wes_palette(name=PalName, n=N, type="continuous")
  }
  names(PAL)<-Names
  
  return(PAL)
}


country_zips<-data.table(filepath=list.files("Data/country_data_zips",".zip",full.names = T))
country_zips[,iso3c:=gsub(".zip|Data/country_data_zips/|-annual|-seasonal","",filepath)][,timeframe:=gsub(".zip","",unlist(tail(tstrsplit(unlist(tail(tstrsplit(filepath,"/"),1)),"-"),1)))]
country_zips[,folder:=gsub(".zip","",unlist(tail(tstrsplit(filepath,"/"),1)))]
country_zips[,Country:=countrycode::countrycode(iso3c, origin = 'iso3c', destination = 'country.name')]

hazards<-c("NDD","NTx40","HSM_NTx35","HSH_max","HSH_mean","THI_max","THI_mean","NDWS","TAI","NDWL0","PTOT")
haz_meta<-data.table::fread("./Data/metadata/haz_metadata.csv")
haz_class<-fread("./Data/metadata/haz_classes.csv")
haz_classes<-unique(haz_class$description)

Scenarios<-c("ssp245","ssp585")
Times<-c("2021_2040","2041_2060")
Scenarios<-rbind(data.table(Scenario="historic",Time="historic"),data.table(expand.grid(Scenario=Scenarios,Time=Times)))

scenarios_x_hazards<-data.table(Scenarios,Hazard=rep(hazards,each=nrow(Scenarios)))[,Scenario:=as.character(Scenario)][,Time:=as.character(Time)]

country_choice<-"Tanzania"
timeframe_choice<-"seasonal"

country_dir<-paste0("./Data/", country_zips[Country==country_choice & timeframe==timeframe_choice,folder])

  if(!dir.exists(country_dir)){
    dir.create(country_dir)
    unzip(zipfile=country_zips[Country==country_choice & timeframe==timeframe_choice,filepath],exdir=country_dir,junkpaths=T)
  }

SaveDir<- paste0(country_dir,"/Analysis")

if(!dir.exists(SaveDir)){
    dir.create(SaveDir)
  }

  Geographies<-list(
    admin2=terra::aggregate(terra::vect(grep("_2.shp",list.files(country_dir,full.names = T),value = T)),by="NAME_2"),
    admin1=terra::aggregate(terra::vect(grep("_1.shp",list.files(country_dir,full.names = T),value = T)),by="NAME_1"),
    admin0=terra::vect(grep("_0.shp",list.files(country_dir,full.names = T),value = T))
  )
  
  # Create standard name field for each admin vector
  Geographies$admin2$admin_name<-Geographies$admin2$NAME_2
  Geographies$admin1$admin_name<-Geographies$admin1$NAME_1
  Geographies$admin0$admin_name<-Geographies$admin0$COUNTRY

Exposure<-terra::rast(paste0(country_dir,"/Exposure.tif"))





Cropland<-Exposure$`cropland-area-ha`
names(Cropland)<-"Cropland_Area"

TotalPop<-Exposure$`rural_pop-total-k`
names(TotalPop)<-"TotalPop"

SPAM<-Exposure[[grep("harvested_area|vop_total|production-mt",names(Exposure),value = T)]]

GLW<-Exposure[[grep("goats|pigs|cattle|buffalo|sheep|livestock|chickens|horses",names(Exposure),value = T)]]

SPAM_GLW<-Exposure[[!grepl("rural_pop|cropland-area",names(Exposure))]]

exposure_layers<-data.table(layer_name=names(SPAM_GLW))
exposure_layers[,product:=unlist(tstrsplit(names(SPAM_GLW),"-",keep=1))
                ][,variable:=unlist(tstrsplit(names(SPAM_GLW),"-",keep=2))
                  ][,unit:=unlist(tstrsplit(names(SPAM_GLW),"-",keep=3))]

exposure_layers[,category:="crop"][product %in% c("goats","sheep","pigs","cattle","chickens","horses","livestock","buffalo"),category:="livestock"]


haz_names<-hazards[hazards != "NDD"]
Analysis_Vars<-haz_names[c(1,4,6)]
Plot_Vars<-Analysis_Vars

AdminLevel<-"Admin2"
Admin1<-Geographies$admin1$admin_name
Admin2<-Geographies$admin2$admin_name

Future<-"ssp245-2041_2060"
PropThreshold<-0.5
Palette<-"turbo"
borderwidth<-1

SubGeog<-if(AdminLevel=="Admin2"){
    Geographies$admin2[Geographies$admin2$admin_name %in% Admin2,]
  }else{
    Geographies$admin1[Geographies$admin1$admin_name %in% Admin1,]
  }

SubGeog_rast<-rasterize(SubGeog,PlotHazards,field="admin_name")

SxRtab_hist<-SxRtabFun(Hazards=PlotHazards,
               Plot_Vars=Plot_Vars,
               ExtractBy=SubGeog_rast)[,Scenario:="historic"]
  

SxRtab_future<-SxRtabFun(Hazards=PlotHazards_future,
               Plot_Vars=Plot_Vars,
               ExtractBy=SubGeog_rast)[,Scenario:=Future]
  
SxRtab_diff<-SxRtabFun(Hazards=PlotHazards_future-PlotHazards,
                         Plot_Vars=Plot_Vars,
                         ExtractBy=SubGeog_rast)[,Scenario:="difference"]


SxRtab<-rbind(SxRtab_hist,SxRtab_future,SxRtab_diff)[,c(11,1:10)]

SxRtab_plot<-melt(SxRtab,id.vars = c("Scenario","admin","Hazard"))
SxRtab_plot[grepl("-",variable),error:=unlist(tstrsplit(variable,"-",keep=2))
            ][,variable:=unlist(tstrsplit(variable,"-",keep=1))
              ][is.na(error),error:="value"]
SxRtab_plot<-dcast(SxRtab_plot,Scenario+admin+Hazard+variable~error)

SxRtab_plot_ss<-SxRtab_plot[admin == admin[1]]

haz_drop<-SxRtab_plot_ss[,list(rmhaz=all(value==0)),by=Hazard][rmhaz==T,Hazard]

SxRtab_plot_ss<-SxRtab_plot_ss[!Hazard %in% haz_drop]

g<-ggplot(data=SxRtab_plot_ss, aes(x = Scenario, y = value,fill=Scenario)) + 
  facet_wrap(Hazard~variable,scales = "free",ncol=4)+
  geom_bar(stat = "identity",position = position_dodge(width = 0.9))+
  geom_errorbar(aes(x=Scenario, ymin=value-sd, ymax=value+sd), width=0.4, colour="black",alpha=0.9, size=0.5)+
  theme_minimal()+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))
g

Thresholds<-data.table(Variable=haz_class[description=="Severe",index_name],
                       Renamed=haz_class[description=="Severe",index_name],
                       Threshold=haz_class[description=="Severe",lower_lim],
                       Direction=rep(">",haz_class[description=="Severe",.N]))

FileName<-gsub("<","L",gsub(">","G",paste(unlist(Thresholds[,!"Renamed"]),collapse="")))
FileName2<-gsub("<","L",gsub(">","G",paste(unlist(Thresholds[Renamed %in% Analysis_Vars,!"Renamed"]),collapse="")))

 
Hazards<-HazardWrapper(Thresholds=Thresholds,
                       FileName=FileName,
                       SaveDir=SaveDir,
                       PropThreshold=PropThreshold,
                       PropTDir=">",
                       hazard_dir= country_dir,
                       Scenarios=Scenarios)


PlotHazards<-terra::mask(terra::crop(Hazards[["historic-historic"]],SubGeog),SubGeog)
PlotHazards_future<-terra::mask(terra::crop(Hazards[[Future]],SubGeog),SubGeog)
PlotHazards_diff<-PlotHazards_future- PlotHazards

HazComb<- HazCombWrapper(Hazards=Hazards,
                 SaveDir=SaveDir,
                 Scenarios=Scenarios,
                 FileName=FileName2,
                 SelectedHaz = Analysis_Vars)

HazXRegion<-HazXRegionWrapper(SaveDir=SaveDir,
                    FileName=FileName2,
                    Exposure=SPAM_GLW,
                    Cropland=Cropland,
                    TotalPop=TotalPop,
                    Geographies=Geographies,
                    HazComb=HazComb)



HazPalCombMean<-PalFun(PalName=Palette,
                                 N=nrow(HazComb[["MeanHaz"]][[Future]][["Classes"]]),
                                 Names=HazComb[["MeanHaz"]][[Future]][["Classes"]][["Hazard"]])


HazPalCombProp<-PalFun(PalName=Palette,
                                 N=nrow(HazComb[["PropHaz"]][[Future]][["Classes"]]),
                                 Names=HazComb[["PropHaz"]][[Future]][["Classes"]][["Hazard"]])



addGeog1<-function(){terra::plot(terra::aggregate(SubGeog,by="NAME_1"),add=T,border="black",lwd=borderwidth)}
addGeog2<-function(){terra::plot(SubGeog,add=T,border="black",lwd=borderwidth)}

historic<-PlotHazards[[paste0(Plot_Vars,"_mean")]]
future<-PlotHazards_future[[paste0(Plot_Vars,"_mean")]]
names(historic)<-paste0("historic-",names(historic))
names(future)<-paste0(Future,"-",names(future))
SR_plot1_mean<-c(historic,future)

HazMeth<-"MeanHaz"


DT_Data_historic<-
  PrepTable(Data=HazXRegion,
            Method=HazMeth,
            Scenario="historic-historic",
            AdminLevel=AdminLevel,
            Geographies=Geographies,
            A1=Admin1,
            A2=Admin2,
            Table="CropPop")[,Scenario:="historic"]


DT_Data_future<-
  PrepTable(Data=HazXRegion,
            Method=HazMeth,
            Scenario=Future,
            AdminLevel=AdminLevel,
            Geographies=Geographies,
            A1=Admin1,
            A2=Admin2,
            Table="CropPop")[,Scenario:=Future]

setnames(DT_Data_historic,c("Cropland_Risk_Perc","Pop_Risk_Perc"),c("Past_Crop_Risk_Pect","Past_Pop_Risk_Perc"))
setnames(DT_Data_future,c("Cropland_Risk_Perc","Pop_Risk_Perc"),c("Fut_Crop_Risk_Pect","Fut_Pop_Risk_Perc"))

DT_Data_historic[,Scenario:=NULL]
DT_Data_future[,Scenario:=NULL]

DT_Data_diff<-merge(DT_Data_historic,DT_Data_future,all.x=T)
DT_Data_diff[is.na(Admin1.x),Admin1.x:=Admin1.y][,Admin1.y:=NULL]
setnames(DT_Data_diff,"Admin1.x","Admin1")

DT_diff_fun<-function(historic,future){
  classes<-sapply(historic,class)
  cols_in<-names(classes)[classes %in% c("numeric","integer")]
  cols_out<-names(classes)[!classes %in% c("numeric","integer")]
  cols_out<-cols_out[cols_out!="Scenario"]
  x<-future[,..cols_in]-historic[,..cols_in]
  x[,Scenario:="difference"]
  x<-c(historic[,..cols_out],x)
  return(x)
}

DT_Data<-DT_diff_fun(historic=DT_Data_historic,future=DT_Data_future)


# Check hazard layers
country_zips<-data.table(filepath=list.files("Data/country_data_zips",".zip",full.names = T))
country_zips[,iso3c:=gsub(".zip|Data/country_data_zips/|-annual|-seasonal","",filepath)][,timeframe:=gsub(".zip","",unlist(tail(tstrsplit(unlist(tail(tstrsplit(filepath,"/"),1)),"-"),1)))]
country_zips[,folder:=gsub(".zip","",unlist(tail(tstrsplit(filepath,"/"),1)))]
country_zips[,Country:=countrycode::countrycode(iso3c, origin = 'iso3c', destination = 'country.name')]

country_choice<-"Tanzania"
timeframe_choice<-"seasonal"

country_dir<-paste0("./Data/", country_zips[Country==country_choice & timeframe==timeframe_choice,folder])

if(!dir.exists(country_dir)){
  dir.create(country_dir)
  unzip(zipfile=country_zips[Country==country_choice & timeframe==timeframe_choice,filepath],exdir=country_dir,junkpaths=T)
}


haz_files<-list.files(country_dir,".tif",full.names = T)
haz_files<-grep("historical|ENSEMBLE",haz_files,value=T)

# last 6 years are corrupted in Angola, for Burundi only the last year, djibouti in zero years, Tanzania 7 years 
years<-as.character(2006:2013)

plot(terra::rast(haz_files[1])[[years]])
plot(terra::rast(haz_files[2])[[years]])
plot(terra::rast(haz_files[3])[[years]])
plot(terra::rast(haz_files[4])[[years]])
plot(terra::rast(haz_files[5])[[years]])
plot(terra::rast(haz_files[6])[[years]])
plot(terra::rast(haz_files[7])[[years]])
plot(terra::rast(haz_files[8])[[years]])
plot(terra::rast(haz_files[9])[[years]])
plot(terra::rast(haz_files[10])[[years]])
plot(terra::rast(haz_files[11])[[years]]) # OK = TAI
plot(terra::rast(haz_files[12])[[years]])
plot(terra::rast(haz_files[13])[[years]])
plot(terra::rast(haz_files[14])[[years]])

# last 6 years are corrupted
years<-as.character(2033:2039)
plot(terra::rast(haz_files[15])[[years]])
plot(terra::rast(haz_files[16])[[years]])
plot(terra::rast(haz_files[17])[[years]])
plot(terra::rast(haz_files[18])[[years]])
plot(terra::rast(haz_files[19])[[years]])
plot(terra::rast(haz_files[20])[[years]])
plot(terra::rast(haz_files[21])[[years]])
plot(terra::rast(haz_files[22])[[years]])
plot(terra::rast(haz_files[23])[[years]])

file_picked<-choose.files()
plot(terra::rast(file_picked)[[years]])

# Exposure summary
SPAM_GLW_SubGeog<-terra::mask(terra::crop(SPAM_GLW,SubGeog),SubGeog)

exposure_variable<-sort(unique(exposure_layers$variable))[1]
exposure_unit<-exposure_layers[variable==exposure_variable,unique(unit)]

Exposure_plot<-SPAM_GLW_SubGeog[[grepl(exposure_variable,names(SPAM_GLW_SubGeog))]]

Vals<-unlist(lapply(1:nlyr(Exposure_plot),FUN=function(i){sum(Exposure_plot[[i]][],na.rm=T)}))
names(Vals)<-unlist(tstrsplit(names(Exposure_plot),"-",keep=1))
Vals<-sort(Vals,decreasing=T)

exposure_crops<-names(Vals)[1:8]

Exposure_plot2<-Exposure_plot[[names(Exposure_plot) %in% paste0(exposure_crops,"-",exposure_variable,"-",exposure_unit)]]
plot(Exposure_plot2)

# Trends
data_ex1<-trends_extract(Geographies=Geographies,
                 scenarios_x_hazards=scenarios_x_hazards,
                 DataDir=country_dir,
                 SaveDir=SaveDir,
                 haz_class=haz_class)

data_ex<-data_ex1[,haz_name:=haz_names[match(data_ex1$hazard,hazards)]]

g_hist<-trend_plot_line(data=data_ex,
                  haz_class=haz_class,
                  haz_choice="PTOT",
                  admin_choice=data_ex[,unique(admin_name)][1:3],
                  scenario_choice="historic",
                  adminlevel_choice=tolower(AdminLevel),
                  palette_choice=Palette)

plot(g_hist)
