
require("plyr")
require("lubridate")
library("ggplot2")
library("plotly")
require("httr")
require("jsonlite")
require("data.table")


DBG<- TRUE # For debugging purposes
GRAYLOG_SERVER <- "160.85.2.244:12900"
LIMIT_RECORDS <- 80000

DBG_LOCAL_DATA <- FALSE # for local testing

levels_so_phase <- c("init", "activate", "deploy", "provision", "update", "retrieve", "destroy")
layout_so_phase <- c(1,7)
#MCN_levels_so_phase <- c("Deploy", "Deploy", "Deploy", "Provision", "Operation", "retrieve", "Disposal")
MCN_levels_so_phase <- c("Deploy", "Deploy", "Deploy", "Provision", "Provision", "retrieve", "Disposal")


MCN_levels_so_phase_unique <- c("Deploy", "Provision", "Operation", "Disposal") # As per D2.2
MCN_layout_so_phase <- c(1,4)


#Deployment, Provisioning, Operation & Runtime Management and Disposal
demo_e2e_services <- c('demo-e2e', 'ims', 'dnsaas', 'maas', 'epc', 'rcb', 'ranoai', 'andsfaas', 'imsaas', 'sla', 'aaa', 'icnaas', 'dssaas', 'ranoai-fokus' )
#levels_demo_e2e <- c("demo-e2e", "aaa", "andsfaas", "dnsaas", "dssaas", "epc", "icnaas",  "ims", "maas",  "ranoai", "rcb", "sla"  )
levels_demo_e2e <- c("demo-e2e", "aaa", "andsfaas", "dnsaas", "dssaas", "epc", "icnaas",  "ims", "maas",  "ranoai", "ranoai-fokus",  "sla"  )
layout_demo_e2e <- c(1,12)
demo_services_scaling <- c('dnsaas', 'imsaas', 'icnaas', 'dssaas')


options("digits.secs"=6)
#options(shiny.fullstacktrace = FALSE)
#options(shiny.error = browser)
#options(shiny.trace = FALSE)



MyOptTHEME  <- theme( plot.title=element_text(size=14), 
                      axis.text.x = element_text(angle=0,  size=12, colour="black"), 
                      axis.text.y = element_text(angle=0,size=12, colour="black", hjust=0.9 ) , 
                      strip.text.x=element_text(size=10), 
                      strip.text.y=element_text(size=10), 
                      axis.ticks=element_line(),
                      panel.background = element_rect(fill = "white", colour = NA), 
                      panel.grid.minor = element_line(linetype = "dotted", colour="grey85", size=0.2 ),
                      panel.grid.major = element_line(linetype = "dotted", colour="grey85", size=0.2 ),
                      axis.line=element_line(),
                      axis.title.x=element_text(size=12),
                      axis.title.y=element_text(size=12, angle=90),
                      legend.position="none"
)




mydbg <- function(istr=NULL){
  if (!is.null(istr)) {
    if (DBG){ 
      if (length(istr) >0 ){
        cat(istr)
      }
    }
  }
}


get_data_from_graylog <- function(start_period=NA, end_period=NA, user_, pass_){
  
  if (is.na(start_period)){
    aux_temp <- Sys.time() - 3600 # One Hour less
    start_period <- paste0(Sys.Date(), " ", format(aux_temp, "%H:%M:%S")) 
  }
  
  if (is.na(end_period)){
    end_period <- paste0(Sys.Date(), " ", format(Sys.time(), "%H:%M:%S"))
  }
  
  str_start_period <- paste0(start_period )
  str_end_period <- paste0(end_period)
  
  dt <- NULL
  if (DBG_LOCAL_DATA){
    load("DATA")
    #filter by dates
    dd_all$tim <- ymd_hms(dd_all$timestamp)
    dd_all <- dd_all[dd_all$tim >= str_start_period & dd_all$tim <= str_end_period,]
    if (!is.null(dd_all) && dim(dd_all)[1] > 0){
      if ("scaling" %in% colnames(dd_all) ){
        mydbg("with scaling")
        dt <- dd_all[,c("source", "sm_name",  "so_phase", "phase_event", "response_time", "tenant", "timestamp", "scaling")]
      } else  {
        dt <- dd_all[,c("source", "sm_name",  "so_phase", "phase_event", "response_time", "tenant", "timestamp")]
      }
    }else{
      dt <- NULL
    }
    rm(dd_all)
  
  }else{
      
    str_aux <- paste0("http://",GRAYLOG_SERVER,"/search/universal/absolute") 
    
    aa <- GET(str_aux, 
              query=list(query="_exists_:sm_name", from=str_start_period, to=str_end_period, limit=LIMIT_RECORDS)
              ,accept_json(), content_type_json(),
              authenticate(user_, pass_))  
    
    #mydbg(headers(aa))
    
    cont_raw <- content(aa, "text")
    
    if (validate(cont_raw) ){
      raw <- fromJSON(cont_raw)  
      dt_raw <- data.frame(raw$messages$message)
      mydbg("Valid content")
      mydbg(str(dt_raw))
      
      if (!is.null(dt_raw) && dim(dt_raw)[1] > 0){
        mydbg(colnames(dt_raw))
        #mydbg(str(dt_raw))
        if ("scaling" %in% colnames(dt_raw) ){
          mydbg("with scaling")
          dt <- dt_raw[,c("source", "sm_name",  "so_phase", "phase_event", "response_time", "tenant", "timestamp", "scaling")]
        } else  {
          dt <- dt_raw[,c("source", "sm_name",  "so_phase", "phase_event", "response_time", "tenant", "timestamp")]
        }
      }else {
        dt <-NULL
      }
    }
  
  }
  return(dt)
}


f_time_plot <- function(data_graylog, rnd.number=0){
  aret <- round(hour(data_graylog$tim)*3600 + minute(data_graylog$tim) * 60 + second(data_graylog$tim) ,rnd.number ) 
  return(aret)
}


f_to_timestamp <- function(idf){
  aa <- as.numeric(as.POSIXlt(idf, format="%Y-%m-%dT%H:%M:%S"))
  return(aa)
}

f_replace_so_phases <-function(idf, services, rm.retrieve=TRUE, rm.disposal=TRUE, all.sources=FALSE){
  ini_levels=levels_so_phase
  final_levels=MCN_levels_so_phase
  
  len <- length(ini_levels)
  
  id_ <- idf
  if (rm.retrieve){
    id_ <- idf[idf$so_phase != "retrieve", ]
  }
  
  if (rm.disposal){
    id_ <- id_[id_$so_phase != "destroy", ]
  }
  
  if (all.sources){
    id_ <- id_
  }else{
    id_ <- id_[id_$source == "mcn-sms", ]
  }
  
  #if (!is.null(id_) & dim(id_)[1] > 0){
  #mydbg("LEN of vec in replaceso ", dim(id_)[1])
  id_$so_phase_mcn <- NA
  #}
  
  if (len == length(final_levels)){
    
    for (i in seq(from=1, by=1, to=len)){
      temp <- ini_levels[i]
      if ( dim(id_[id_$so_phase==temp,])[1] > 0 ){
        id_[id_$so_phase==temp,"so_phase_mcn"] <- as.character(final_levels[i])
      }
    } 
    
    #
    #First update
    #
    for (sm in services){
      mydbg(paste0("\n Checking for service == ", sm))
      a_update <- na.omit(id_[id_$sm_name==sm & id_$so_phase == "update" & id_$phase_event == "start" & id_$source=="mcn-sms",])
      aux_0 <- dim(a_update)[1]
      if (aux_0 > 0){
        min_upd_start <- min(id_[id_$sm_name==sm & id_$so_phase == "update" & id_$phase_event == "start" & id_$source=="mcn-sms", "time_plot"])
        min_upd_end <- min(id_[id_$sm_name==sm & id_$so_phase == "update" & id_$phase_event == "done" & id_$source=="mcn-sms", "time_plot"])
        
        aux <- dim(id_[id_$sm_name == sm & id_$so_phase == "update" & id_$source=="mcn-sms" & id_$time_plot >= min_upd_start & id_$time_plot <= min_upd_end, ])
        
        # Remove updates that included in the provisioning....
        id_ <- id_[! (id_$sm_name == sm & id_$so_phase == "update" & id_$source!="mcn-sms" & id_$time_plot >= min_upd_start & id_$time_plot <= min_upd_end), ]
        
        if (aux[1]>0){
          id_[id_$sm_name == sm & id_$so_phase == "update" & id_$source=="mcn-sms" & id_$time_plot >= min_upd_start & id_$time_plot <= min_upd_end, "so_phase_mcn" ] <- "Provision"
        }
      }
    }
    
    id_$so_phase_mcn <- factor(id_$so_phase_mcn, levels=MCN_levels_so_phase_unique)
    
    return (id_)
  } 
}


calc_duration <-function(idf, services ){
  # remove NAs at specific column
  ida<- idf[!is.na(idf$sm_name),]
  ida$duration <- 0
  for (sm in services){
    for (phase in levels(idf$so_phase)){
      mydbg(paste0("\n DEtermining duration for phase ", phase, " of sm ", sm))
      start_time <- ida[ida$sm_name==sm & ida$so_phase==phase & ida$phase_event=="start", "time_plot2"]
      aux_ <- dim(ida[ida$sm_name==sm & ida$so_phase==phase & ida$phase_event %in% c("done", "finished"), ])
      
      if (aux_[1] != length(start_time) ){
        #print(paste("error at dimensions done=", aux_[1], " and start=", length(start_time)))
        #print(idf[idf$sm_name==sm & idf$so_phase==phase & idf$phase_event %in% c("done", "finished"), ])
        #print(start_time)
      }else{
        ida[ida$sm_name==sm & ida$so_phase==phase & ida$phase_event %in% c("done", "finished"), "duration"]  <- ida[ida$sm_name==sm & ida$so_phase==phase & ida$phase_event %in% c("done", "finished"), "time_plot2"] - start_time
      }
    }
  }
  
  return (ida)
}


f_grapg_plot_mcn <- function(dg_mcn){
  aa <-ddply(.data=dg_mcn[dg_mcn$phase_event=="start" ,], .(sm_name, so_phase_mcn, phase_event, response_time), .fun=summarize, tim=min(tim))
  ab<- ddply(.data=dg_mcn[dg_mcn$phase_event %in% c("done", "finished") ,], .(sm_name, so_phase_mcn, phase_event, response_time), .fun=summarize, tim=max(tim) )
  
  aa <- rbind(aa, ab)
  
  aa <- aa[with(aa, order(tim )), ]
  aa$time_plot <- f_time_plot(aa)
  aa$time_graph <- -1
  #Determine time_elapsed
  amin <- min(aa$time_plot)
  
  #Not optimized... buyt works
  for (i in seq(from=1, to=dim(aa)[1], by=1)){
    aa[i,"time_graph"] <- aa[i, "time_plot"] - amin
  }
  
  return(aa)
}


f_calc_duration_phases_mcn <-function(idf, services, levels_services ){
  
  id_ <- NULL
  for (sm in services){
    for (phase in levels(idf$so_phase_mcn)){
      a_start_time <- min(idf[idf$sm_name==sm & idf$so_phase_mcn==phase & idf$phase_event=="start" & idf$source=="mcn-sms", "time_plot2" ])
      df_start_time <- idf[idf$time_plot2==a_start_time, ]
      if (dim(df_start_time)[1] > 0){
        a_end_time <- max(idf[idf$sm_name==sm & idf$so_phase_mcn==phase & idf$phase_event %in% c("done", "finished") & idf$source=="mcn-sms" , "time_plot2"])
        df_end_time <- idf[idf$time_plot2==a_end_time, ]
        
        aux_start <- data.frame(timestamp=df_start_time$timestamp, source=df_start_time$source, sm_name=sm, so_phase_mcn=phase, phase_event="start", response_time=df_start_time$response_time, timestamp_unix=df_start_time$timestamp_unix, time_plot2=df_start_time$time_plot2, duration=0)
        aux_end <- data.frame(timestamp=df_end_time$timestamp, source=df_end_time$source, sm_name=sm, so_phase_mcn=phase, phase_event="done", response_time=df_end_time$response_time, timestamp_unix=df_end_time$timestamp_unix, time_plot2=df_end_time$time_plot2, duration=df_end_time$time_plot2 - df_start_time$time_plot2)
        id_ <- rbind(id_, aux_start)
        id_ <- rbind(id_, aux_end)
      }
    }
  }
  
  if (!is.null(id_)){
    id_$so_phase_mcn <- factor(id_$so_phase_mcn, levels=MCN_levels_so_phase_unique)
    id_$sm_name <- factor(id_$sm_name, levels=levels_services)
  }
  return (id_)
}


# Function to prepare data for statisitcs of lifecycles in MCN
f_stats_lifeCycle_mcn <-function(idf, services, rm.retrieve=FALSE, rm.disposal=TRUE, all.sources=FALSE){
  ini_levels=levels_so_phase
  final_levels=MCN_levels_so_phase
  
  len <- length(ini_levels)
  
  id_ <- idf
  if (rm.retrieve){
    id_ <- idf[idf$so_phase != "retrieve", ]
  }
  
  if (rm.disposal){
    id_ <- id_[id_$so_phase != "destroy", ]
  }
  
  if (all.sources){
    id_ <- id_
  }else{
    id_ <- id_[id_$source == "mcn-sms", ]
  }
  
  id_ <- id_[id_$sm_name %in% services, ]
  
  id_$so_phase_mcn <- NA
  
  
  if (len == length(final_levels)){
    
    for (i in seq(from=1, by=1, to=len)){
      temp <- ini_levels[i]
      if ( dim(id_[id_$so_phase==temp,])[1] > 0 ){
        id_[id_$so_phase==temp,"so_phase_mcn"] <- as.character(final_levels[i])
      }
    } 
    
    id_$so_phase_mcn <- factor(id_$so_phase_mcn, levels=MCN_levels_so_phase_unique)
    
    return (id_)
  } 
}



##
# Ggplot2 
#
g2_graph_so_phase_mcn <-function(dg, scale_x=NULL, so_phases_mcn=MCN_levels_so_phase_unique, do.summary=TRUE){
  
  #do some filtering
  dg <- dg[dg$so_phase_mcn %in% so_phases_mcn, ]
  
  if (do.summary){
    aa <-ddply(.data=dg[dg$phase_event=="start" ,], .(sm_name, so_phase_mcn, phase_event), .fun=summarize, time_graph=min(time_graph))
    ab<- ddply(.data=dg[dg$phase_event %in% c("done", "finished") ,], .(sm_name, so_phase_mcn, phase_event), .fun=summarize, time_graph=max(time_graph) )
    dg <- rbind(aa, ab)
  }
  
  gl_bw_ser <- ggplot(data=dg, aes(x=time_graph, y=sm_name, group=sm_name, colour=sm_name))
  gl_bw_ser <- gl_bw_ser + geom_path(lineend = "round", linejoin = "round", size=1.1)
  
  if (do.summary){
    gl_bw_ser <- gl_bw_ser + geom_point(size=3, shape=1)
  }else{
    gl_bw_ser <- gl_bw_ser + geom_point(size=3, aes(shape=phase_event))
  }
  gl_bw_ser <- gl_bw_ser + xlab("Time(s) ")
  gl_bw_ser <- gl_bw_ser + ylab("Services")
  gl_bw_ser <- gl_bw_ser + MyOptTHEME
  if (!is.null(scale_x) ){
    gl_bw_ser <- gl_bw_ser + scale_x_continuous(breaks=scale_x)
  }
  gl_bw_ser <- gl_bw_ser + facet_wrap(~so_phase_mcn, ncol=1, scales="fixed", as.table = FALSE)
  return (gl_bw_ser)  
}


g2_graph_so_phase_mcn_bytim <-function(dg, scale_x=NULL, so_phases_mcn=MCN_levels_so_phase_unique, do.summary=TRUE){
  
  #do some filtering
  dg <- dg[dg$so_phase_mcn %in% so_phases_mcn, ]
  
  if (do.summary){
    aa <-ddply(.data=dg[dg$phase_event=="start" ,], .(sm_name, so_phase_mcn, phase_event), .fun=summarize, tim=min(tim))
    ab<- ddply(.data=dg[dg$phase_event %in% c("done", "finished") ,], .(sm_name, so_phase_mcn, phase_event), .fun=summarize, tim=max(tim) )
    dg <- rbind(aa, ab)
  }
  
  gl_bw_ser <- ggplot(data=dg, aes(x=tim, y=sm_name, group=sm_name, colour=sm_name))
  gl_bw_ser <- gl_bw_ser + geom_path(lineend = "round", linejoin = "round", size=1.1)
  
  if (do.summary){
    gl_bw_ser <- gl_bw_ser + geom_point(size=3, shape=1)
  }else{
    gl_bw_ser <- gl_bw_ser + geom_point(size=3, aes(shape=phase_event))
  }
  gl_bw_ser <- gl_bw_ser + xlab("Time(s) ")
  gl_bw_ser <- gl_bw_ser + ylab("Services")
  gl_bw_ser <- gl_bw_ser + MyOptTHEME
  if (!is.null(scale_x) ){
    gl_bw_ser <- gl_bw_ser + scale_x_continuous(breaks=scale_x)
  }
  gl_bw_ser <- gl_bw_ser + facet_wrap(~so_phase_mcn, ncol=1, scales="fixed", as.table = FALSE)
  return (gl_bw_ser)  
}

g2_graph_response_time_per_so_phase_mcn <-function(dg, scale_y=NULL, rm.operation=TRUE, is.update=FALSE, type="duration" ){
  
  if (rm.operation){
    dg <- dg[dg$so_phase_mcn != "Operation",]
  }
  
  if (type=="duration"){
    gl_bw_ser <- ggplot(data=dg, aes(x=so_phase_mcn, y=duration, fill=sm_name))  
  }else{
    gl_bw_ser <- ggplot(data=dg, aes(x=so_phase_mcn, y=response_time, fill=sm_name))
  }
  if (is.update){
    gl_bw_ser <- gl_bw_ser + geom_boxplot(position="dodge", colour="black", outlier.shape = NA)
  }else{
    gl_bw_ser <- gl_bw_ser + geom_bar(position="dodge", stat="summary", fun.y="sum", colour="black")
  }
  gl_bw_ser <- gl_bw_ser + xlab("Lifecyle phase per service ")
  
  if (is.update){
    gl_bw_ser <- gl_bw_ser + ylab("Mean response time (s)")
  }else{
    gl_bw_ser <- gl_bw_ser + ylab("Response time (s)")
  }
  gl_bw_ser <- gl_bw_ser + MyOptTHEME
  if (!is.null(scale_y)){
    gl_bw_ser <- gl_bw_ser + scale_y_continuous(breaks=scale_y)
  }
  gl_bw_ser <- gl_bw_ser + scale_fill_discrete(guide=guide_legend(title="Services"))
  gl_bw_ser <- gl_bw_ser + theme(legend.position="top", 
                                 legend.direction="horizontal", 
                                 legend.background = element_rect(colour="black", size=0.5)) 
  
  return (gl_bw_ser)  
}


#
# Stats per Lifecycle phase
#
g2_graph_stats_lifecycle_mcn <-function(dg,rm.start=TRUE, rm.responstime0=TRUE ){
  
  id_ <- dg
  if (rm.start){
    id_ <- id_[id_$phase_event != "start",]
  }
  
  if (rm.responstime0){
    id_ <- id_[id_$response_time != 0, ]
  }
  
  gl_bw_ser <- ggplot(data=id_, aes(x=so_phase_mcn, y=response_time, fill=so_phase_mcn ))
  gl_bw_ser <- gl_bw_ser + geom_boxplot(position="dodge", notch=FALSE, outlier.shape = NA)

  gl_bw_ser <- gl_bw_ser + xlab("MCN Life Cycle Phases ")
  gl_bw_ser <- gl_bw_ser + ylab("Time (s)")
  gl_bw_ser <- gl_bw_ser + MyOptTHEME
  
  gl_bw_ser <- gl_bw_ser + scale_fill_discrete(guide=guide_legend(title="Services"))
  
  return (gl_bw_ser)  
}



g2_graph_stats_lifecycle <-function(dg, rm.start=TRUE, rm.responstime0=TRUE, service=NA ){
  
  id_ <- dg
  
  #Get specific service
  if (!is.na(service)){
    id_ <- id_[id_$sm_name == service,]
  }
  
  if (rm.start){
    id_ <- id_[id_$phase_event != "start",]
  }
  
  if (rm.responstime0){
    id_ <- id_[id_$response_time != 0, ]
  }
  
  gl_bw_ser <- ggplot(data=id_, aes(x=factor(so_phase), y=response_time, fill=factor(so_phase) ))
  gl_bw_ser <- gl_bw_ser + geom_boxplot(position="dodge", notch=FALSE, outlier.shape = NA)
  
  gl_bw_ser <- gl_bw_ser + xlab("Service(s) Life Cycle Phases ")
  gl_bw_ser <- gl_bw_ser + ylab("Time (s)")
  gl_bw_ser <- gl_bw_ser + MyOptTHEME
  gl_bw_ser <- gl_bw_ser + scale_x_discrete(limits=levels_so_phase)
  
  gl_bw_ser <- gl_bw_ser + scale_fill_discrete(guide=guide_legend(title="Services"))
  
  return (gl_bw_ser)  
}