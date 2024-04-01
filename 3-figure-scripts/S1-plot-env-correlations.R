#######################################
# WASH Benefits Bangladesh  
# Hydrometeorological risk factors for diarrhea and enteropathogens

# Plot correlations between risk factors in manuscript 

# code adapted from 
# https://github.com/ben-arnold/mbita-schisto
#######################################
rm(list=ls())

library(ellipse)

# configure directories, load libraries and base functions
source(paste0(here::here(), "/0-config.R"))

wbb_diarr = readRDS(paste0(clean_offset_data_dir, "washb-bangladesh-merged-diarr_offset.RDS")) %>% 
  filter(intervention == 0) 

rfs = wbb_diarr %>% dplyr::select(
  ppt_week_sum_0weeklag,
  ppt_week_sum_1weeklag,
  ppt_week_sum_2weeklag,
  ppt_week_sum_3weeklag,
  temp_weekavg_0weeklag,
  temp_weekavg_1weeklag,
  temp_weekavg_2weeklag,
  temp_weekavg_3weeklag,
  vpd,
  distance_from_any_surface_water
)
rfs = rfs[!is.na(rfs),]


colnames(rfs) = c(
  "Rainfall\n(0-week lag)",          
  "Rainfall\n(1-week lag)",          
  "Rainfall\n(2-week lag)",          
  "Rainfall\n(3-week lag)",          
  "Temperature\n(0-week lag)",          
  "Temperature\n(1-week lag)",          
  "Temperature\n(2-week lag)",          
  "Temperature\n(3-week lag)",          
  "Vapor pressure\ndeficit",                            
  "Surface water\ndistance"
)


#----------------------------------
# correlation ellipse
#----------------------------------
myellipse<-function(x,y,...){
  maxx <- max(x,na.rm=TRUE)
  minx <- min(x,na.rm=TRUE)
  maxy <- max(y,na.rm=TRUE)
  miny <- min(y,na.rm=TRUE)
  midx <- (maxx+minx)/2
  midy <- (maxy+miny)/2
  corxy <- cor(x,y,method="spearman",use="pairwise.complete.obs")
  colgroup<-cut(corxy,breaks=seq(-1,1,length=11),labels=F)
  brewcols <- brewer.pal(n=11,"RdYlGn")
  cols<-brewcols[colgroup]
  xyc <-sprintf("%1.2f",corxy)
  xyc[grep("NA",xyc)]<-""
  exy <- ellipse(corxy,centre=c(midx,midy),scale=c((maxx-minx)/6,(maxy-miny)/6))
  polygon(exy,col=alpha(cols,alpha=0.5))
  lines(exy)
  if(!is.na(corxy)) {
    if(corxy<0.8) {
      text(midx,midy,xyc,cex=1.3)
    } else{
      text(maxx,midy-((maxy-miny)/3),xyc,cex=1,adj=1)
    }
  }
  
}

#----------------------------------
# make plot
#----------------------------------
tiff(filename = paste0(fig_dir, "S1-plot-env-correlations.tiff"),
    width=12, height=7, units="in", res=300)
pairs(rfs, cex=0.05,las=1,
      lower.panel=myellipse,
      cex.labels=1.2)
dev.off()


