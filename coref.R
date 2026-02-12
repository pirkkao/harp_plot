# AUXILIARY FUNCTIONS FOR HARP_PLOT PROGRAM
#
#
#

## AUX ##
##

f_name <- function(data,ensm) {
    # Construct correct variable name for ensemble member number "ensm"
    if(length(data$fcst_model)==1){
        nam <- paste0(data$fcst_model,"_mbr",sprintf("%03d",ensm))
    } else {
        nam <- paste0(data$fcst_model[[1]],"_mbr",sprintf("%03d",ensm))
    }

  return(nam)
}

f_eval_expr <- function(to_eval) {
    # An aux function that evaluates expression given in
    # yaml config, e.g. seq(). If a list is given instead that
    # will be returned.

    # Problem: tryCatch "error=" for some reason only returns
    # the first element if a yaml list is provided. Create a 
    # check for list length to prevent this.
    if (length(to_eval)==1){
        to_eval<-tryCatch(expr={eval(parse(text=to_eval))},
                            error=to_eval)
    }
    return(to_eval)
}

## PARSE CONFIG ##
##

f_parse_yaml <- function(exp,cnfg) {

    # Create an empty list to fill
    confs <- list()

    # Loop over items in
    # data:
    #   i:
    for (i in 1:length(exp$data)) {

        # Shorten some names
        tmpl <- exp$data[[i]]$tmpl
        cnfg_tmpl <- cnfg[[tmpl]]

        # Create an empty list
        config <- c()

        # Parse template specific items
        if (is.null(exp$data[[i]]$file_path)) {
            config$file_path <-    cnfg_tmpl$file_path
        } else {
            config$file_path <-    exp$data[[i]]$file_path
        }
        config$file_template <-    cnfg_tmpl$file_template
        config$exps <-             exp$data[[i]]$exp
        config$ensm <-             exp$data[[i]]$ensm
        config$flds <-             exp$data[[i]]$flds
        config$file_format <-      cnfg_tmpl$file_format
        config$file_format_opts <- cnfg_tmpl$file_format_opts
        config$decum<-             exp$data[[i]]$decum
      
        # Parse common time items
        config$sdate <- exp$times$sdate
        config$lead_times <- exp$times$lead_times

        # Parse subgrid options
        if(!is.null(exp$grid$subgrid)){
            config$subgrid <- TRUE
            config$subgrid_opts <- exp$grid$subgrid
        } else {
            config$subgrid <- FALSE
        }

        # Parse regrid options
        if(!is.null(exp$grid$regrid)){
            if((exp$grid$regrid)&(i>1)){
                config$subgrid <- FALSE
                config$regrid <- TRUE
                config$regrid_opts <- ""
            } else {
                config$regrid <- FALSE
            }
        } else { config$regrid<-FALSE }

        # Add to list
        confs[[i]] <- config
    }
        
    # Parse plot options
    #
    # Loop over items in
    # plot:
    #   j:
    #     k:
    #
    config <- c()

    for (j in 1:length(exp$plot)) {
        # Special case for copying the previous rows options
        if(exp$plot[[j]][[1]]$type=="copy"){
            config$plot[[j]]<-config$plot[[j-1]]

        # Normal case, loop over rows and columns
        } else {
            config$plot[[j]]      <- exp$plot[[j]]

            for (k in length(exp$plot[[j]])) {
                config$plot[[j]][[k]] <-          exp$plot[[j]][[k]]
                config$plot[[j]][[k]]$type <-     exp$plot[[j]][[k]]$type
                config$plot[[j]][[k]]$data1 <-    exp$plot[[j]][[k]]$data1
                config$plot[[j]][[k]]$data2 <-    exp$plot[[j]][[k]]$data2
                config$plot[[j]][[k]]$ensm <-     exp$plot[[j]][[k]]$ensm
                config$plot[[j]][[k]]$col_tmpl <- exp$plot[[j]][[k]]$col_tmpl
            }
        }
    }

    # Collect requested color maps
    for (j in 1:length(exp$col_maps)) {
        config$col_maps[[j]]<-              exp$col_maps[[j]]
        config$col_maps[[j]]$name <-        exp$col_maps[[j]]$name
        config$col_maps[[j]]$colours <-     exp$col_maps[[j]]$colours
        config$col_maps[[j]]$direction <-   exp$col_maps[[j]]$direction
        config$col_maps[[j]]$limits <-      exp$col_maps[[j]]$limits
        config$col_maps[[j]]$break_steps <- exp$col_maps[[j]]$break_steps
    }
    # Copy every item from opts and fill
    config$opts <- exp$opts
    config$fill <- cnfg$fill

    # Add to list
    confs[[i+1]] <- config
    
    return(confs)

}


## LOAD DATA ##
##

#
# A function to read in Harmonie files with various options
#
f_load_data <- function(cnfg,fld,expe,regrid_file) {

    # Add options for transformation if requested
    #
    transf<-"none"
    transf_opts<-""
    do_transf<-FALSE
    
    if (cnfg$subgrid) {
    #transf<-"subgrid"
    #transf_opts<-paste0("subgrid_opts(",paste(cnfg$subgrid_opts,collapse=","),")")
        do_transf<-TRUE
        do_transf_opts<-paste0("geo_subgrid(ff,",paste(cnfg$subgrid_opts,collapse=","),")")
    } else if (cnfg$regrid) {
        transf<-"regrid"
        transf_opts<-paste0("regrid_opts(get_domain(regrid_file))")
    }

    # Issues with quotation marks not evaluated from yaml
    file_format<-NULL
    file_format_opts<-list()
    if (cnfg$file_format!="") {
        file_format<-cnfg$file_format
        file_format_opts<-cat(cnfg$file_format_opts,"\n")
    }

    # Parse forecast lengths if they are given as seq()
    lead_times<-f_eval_expr(cnfg$lead_times)
    
    if (FALSE) {
        print(cnfg$sdate)
        print(expe)
        print(cnfg$lead_times)
        print(cnfg$ensm)
        print(cnfg$file_path)
        print(cnfg$file_template)
        print(fld)
        print(file_format)
        print(file_format_opts)
        print(transf)
        print(transf_opts)
        print(eval(parse(text=transf_opts)))
    }
    
    # Fetch data
    #
    ff <- read_forecast(
        cnfg$sdate,
        fcst_model = expe,
        lead_time = lead_times,
        members = cnfg$ensm,
        file_path = cnfg$file_path,
        file_template=cnfg$file_template,
        parameter=fld,
        file_format=file_format,
        file_format_opts=file_format_opts,
        transformation=transf,
        transformation_opts=eval(parse(text=transf_opts)),
        return_data=TRUE
    )

    # Do subgrid if requested
    if (do_transf){
        ff<-eval(parse(text=do_transf_opts))
    }
    
    # Scale temp to celcius
    if (fld == "SURFTEMPERATURE" || fld == "CLSTEMPERATURE" || fld == "t2m" || fld == "air_temperature_2m") {
        ff <- ff |> scale_param(-273.15,"degC")
    }
  
    return (ff)
}


#
# A layer for fetching multiple forecast fields/forecast models.
# Harp FA files don't support multiple fields read-in so need to do it
# this way.
#
f_data_layer <- function(config) {

    # Collect data items into a list
    f <- list()

    idx <- 1
    i <- 1
    # Config elements before the last refer to data
    while (idx <= length(config)-1) {

        # Check for regrid, need to give the geofield to f_load_data if requested
        if ((idx>1) & (config[[idx]]$regrid)) {
            regrid_file=f[[1]][[f_name(f[[1]],config[[1]]$ensm[1])]]
        } else {
            regrid_file=NULL
        }

        # Loops for multiple parameter fetches and multiple exp fetches.
        # NOT TESTED, should work in theory though...
        for (fld in config[[idx]]$flds) {
            for (expe in config[[idx]]$exps) {

                f[[i]] <- f_load_data(config[[idx]],fld,expe,regrid_file)
                i<-i+1
            }
        }
        idx<-idx+1
    }
    return (f)
}

f_decum <- function(f,config){
    # Decumulate the forecast fields.

    # Loop over each opened data source
    for(i in 1:(length(config)-1)){
        
        # Parse forecast lengths if they are given as seq()
        lead_times<-f_eval_expr(config[[i]]$lead_times)

        # Skip if only a single lead time is requested
        if (length(lead_times!=1)) {

            # Check if decumulation is requested
            if(!is.null(config[[i]]$decum)){
                if(config[[i]]$decum){
                        
                    # Assume the lead_times are evenly timed and use a decum 
                    # length for each lead time.
                    decum_t<-lead_times[[2]]-lead_times[[1]]
                    ftemp <- f[[i]] |> decum(decum_t)

                    # For some reason, saving f[[i]]<-f[[i]] |>decum did not work.
                    # Drop 1st lead_time since it's not decumulated.
                    f[[i]] <- ftemp |> filter(lead_time>lead_times[[1]])
                }
            }
        }
    }
            # !COULD NOT GET MERGING OF DF TO WORK, COMMENTED OUT!
            # For uneven lead times, loop over other forecast lead times
            # and do a difference between i+1'th and i'th lead time
            #for (ifclen in 2:length(lead_times)) {
            #    ftemp <- f[[i]] |> filter(lead_time==lead_times[[ifclen-1]])
            #
            #    # Calculate decumation time and then apply decum
            #    ftemp <- ftemp - f[[i]] |> filter(lead_time==lead_times[[ifclen]])
    return(f)
}

f_data_diff <- function(f,config){

    # Copy base for data frame, remove unncessary elements
    # (probably not the best way to do this, but do not know
    #  how else to do it).
    # Need to have this so we can add the difference field to it
    # as g$diff.
    g<-f[[1]]
    for(idx in config[[1]]$ensm){
        g<-g[,! names(g) %in% f_name(g,idx), drop=F ]
    }
    
    # Make difference fields if requested
    plt<-last(config)$plot
    addElem<-FALSE

    # Loop over the rows and columns of plot:i:j
    for (i in 1:length(plt)){
        for(j in 1:length(plt[[i]])) {
            
            # Check if diff is requested
            if(plt[[i]][[j]]$type=="diff"){
                addElem<-TRUE
                d1<-plt[[i]][[j]]$data1
                d2<-plt[[i]][[j]]$data2

                # When taking difference between ensemble members, go here
                if(d1==d2){
                    for(k in plt[[i]][[j]]$ensm){
                        # Only diff for ensm>0
                        # NEED TO REVISIT THIS, maybe taking diff between other members needed?
                        if(k>0) {
                            g[[paste0("diff_",k,d1,d2)]]<-f[[d1]][[f_name(f[[d1]],k)]]-f[[d1]][[f_name(f[[d1]],0)]]
                        }
                    }
                # When taking difference between different data sources, go here.
                # For ensemble stats, calculate the stats before taking diff.
                } else {
                    for(k in plt[[i]][[j]]$ensm){
                        if(k=="ens_mean"){
                            g[[paste0("diff_",k,d1,d2)]]<-ens_stats(f[[d1]])$ens_mean - ens_stats(f[[d2]])$ens_mean
                        } else if(k=="ens_sd"){
                            g[[paste0("diff_",k,d1,d2)]]<-ens_stats(f[[d1]])$ens_sd - ens_stats(f[[d2]])$ens_sd
                        } else {
                            g[[paste0("diff_",k,d1,d2)]]<-f[[d2]][[f_name(f[[d2]],k)]]-f[[d1]][[f_name(f[[d1]],k)]]
                        }
                    }
                }
            }
        }
    }
    # Only add an element to the data list if a difference field was made here
    if(addElem){
        f[[length(f)+1]]<-g
    }
    
    return (f)
}



## END DATA ###
##

p_plot_master <- function(f,config) {

    fh<-list()
    
    # Create colormap setup
    colmaps<-p_colmap_setup(last(config))
    
    # Cycle through the requested rows
    for(i in 1:length(last(config)$plot)) {

        fh[[i]]<-p_plot_layer(f,last(config)$plot[[i]],colmaps)
        i<-i+1
    }
    
    return(fh)
}

p_colmap_setup <- function(cconfig) {

    # Create requested color maps
    #
    sfill<-list()
    legend<-list()
    fm<-cconfig$fill

    # Cycle through the requested color maps and construct
    # necessary input for the scale_fill_template-function.
    # If no specifications are done in the experiment yaml,
    # use presetup values from the common yaml.
    for (i in 1:length(cconfig$col_maps)) {

        cm<-cconfig$col_maps[[i]]

        # Type
        if (is.null(cm$type)) {
            method<-fm[[cm$name]]$type
        } else {
            method<-cm$type
        }
        # Colour
        if (is.null(cm$colours)) {
            colours<-fm[[cm$name]]$colours
        } else {
            colours<-cm$colours
        }
        # Direction
        if (is.null(cm$direction)) {
            direction<-fm[[cm$name]]$direction
        } else {
            direction<-cm$direction
        }
        # Limits
        if (is.null(cm$limits)) {
            limits <- fm[[cm$name]]$limits
        } else {
            limits <- paste0("c(",cm$limits[[1]],",",cm$limits[[2]],")")
        }
        # Breaks
        if (is.null(cm$breaks)) {
            breaks<-fm[[cm$name]]$breaks
        } else {
            breaks<-cm$breaks
        }
        # Other options
        if (is.null(cm$opts)) {
            opts<-fm[[cm$name]]$opts
        } else {
            opts<-cm$opts
        }
        # Generate the colour map
        sfill[[i]]<-scale_fill_template(method,colours,direction,limits,breaks)

        # Also get legend name associated with that colour map here
        if (is.null(cm$legend)) {
            legend[[i]]<-fm[[cm$name]]$legend
        } else {
            legend[[i]]<-cm$legend
        }
    }
    return(list(sfill,legend))
}


p_plot_layer <- function(f,cconfig,colmaps) {

    fhh<-list()
    i<-1
    idx<-1
    sfill<-colmaps[[1]]
    legend<-colmaps[[2]]

    # Cycle through the requested columns to be plotted
    for(idx in 1:length(cconfig)) {

        # Shorten often used variables
        plt <- cconfig[[idx]]
        ff<- f[[plt$data1]]
        
        # Apply colormap if requested
        if (!is.null(plt$col_map)) {
            cmap<-eval(parse(text=sfill[[plt$col_map]]))
        } else {
            cmap<-NULL
        }

        # Create displayed text to plot.
        # NOTE! Could not get facet_wrap to function as wanted so
        # doing this workaround.
        title=NULL
        
        if(plt$type=="fc"){
            for(ensm in plt$ensm) {
                ffname <- f_name(ff,ensm)
                title<-p_plot_title(ff,plt,ensm)
                
                fhh[[i]] <- plot(ff,!!sym(ffname))+labs(fill=legend,title=title)+cmap+
                            theme(plot.title=element_text(hjust=0.5))
                i<-i+1
            }
        } else if(plt$type=="ens_mean") {
            title<-p_plot_title(ff,plt)
            fhh[[i]] <- plot(ens_stats(ff),"ens_mean")+labs(fill=legend,title=title)+cmap+
                        theme(plot.title=element_text(hjust=0.5))
            i<-i+1
        } else if(plt$type=="ens_sd") {
            title<-p_plot_title(ff,plt)
            fhh[[i]] <- plot(ens_stats(ff),"ens_sd")+labs(fill=legend,title=title)+cmap+
                        theme(plot.title=element_text(hjust=0.5))
            i<-i+1
        } else if(plt$type=="diff") {
            title<-p_plot_title(ff,plt)
            # Diff fields should be in the last data element
            ff<-last(f)
            d1<-plt$data1
            d2<-plt$data2

            # Separate into same data diff and not because of ctrl member treatment.
            # NEED TO REVISIT THIS.
            if(d1==d2){
                for(k in plt$ensm){
                    # Only diff for ensm>0
                    if(k>0) {
                        fhh[[i]] <- plot(ff,!!sym(paste0("diff_",k,d1,d2)))+
                                    labs(fill=legend,title=title)+cmap+
                                    theme(plot.title=element_text(hjust=0.5))
                        i<-i+1
                    }
                }
            } else {
                for(k in plt$ensm){
                    fhh[[i]] <- plot(ff,!!sym(paste0("diff_",k,d1,d2)))+
                                labs(fill=legend,title=title)+cmap+
                                theme(plot.title=element_text(hjust=0.5))
                    i<-i+1
                }
            }
               
        }
        idx<-idx+1
    }
    return(fhh)
}

p_plot_title <- function(ff,plt,ensm="") {

    title<-NULL
    exp<-   ""
    mbr<-   ""
    lead<-  ""
    # Construct a title for a plot based on keywords.
    # NEED TO BE EXPANDED.
    if(!is.null(plt$name)){
        for (item in plt$name) {
            if (item=="member") {
                mbr<-paste0("mbr",ensm)
            } else if (item=="lead_time") {
                lead<-paste0("+",ff$lead_time,"h")
            } else {
                exp<-item
            }
        }
        title<-paste(exp,mbr,lead," ")
    }
    return(title)
}
    
    
p_plot_arrange <- function(fh,config) {

    # Create the full subplot row by row
    sh<-list()
    for(i in 1:length(fh)) {
        first<-TRUE
        shh<-fh[[i]][[1]]
        for(j in 1:length(fh[[i]])) {
            if(first) { first<-FALSE } else {   
            shh<-shh+fh[[i]][[j]]
            }
        }
        sh[[i]]<-shh+plot_layout(ncol=j)
        
        # Check if guides are to be collected
        if(!is.null(last(config)$opts$collect_guides)){
            if(last(config)$opts$collect_guides){
                sh[[i]]<-sh[[i]]+plot_layout(guides = "collect")
            }
        } 
    }

    # Stack the rows in vertical
    if(length(sh)==1){
        s<-sh[[1]]
    } else if(length(sh)==2) {
        s<-sh[[1]]/sh[[2]]
    } else if(length(sh)==3) {
        s<-sh[[1]]/sh[[2]]/sh[[3]]
    } else if(length(sh)==4) {
        s<-sh[[1]]/sh[[2]]/sh[[3]]/sh[[4]]
    }
    return(s)
}

p_save_fig <- function(fh,config){

    # Defaults
    fig_name<-"test.png"
    fig_size<-c(15,15,300)

    # Get values from opts if given
    opts<-last(config)$opts

    if (!is.null(opts$fig_name)){fig_name<-opts$fig_name}
    if (!is.null(opts$fig_size)){fig_size<-opts$fig_size}

    # Save
    ggsave(fig_name,fh,width=fig_size[1],height=fig_size[2],dpi=fig_size[3])
    
}

p_plot_beautify <- function(ff,fh,scale_fill_method="scale_fill_fermenter",ffname="default") {

   # Harmonize the colorbars
   #
   bounds <- p_plot_harmonize(ff,0,ffname)
   bounds <- list(10,45)

   scale_fill_method <- paste0(scale_fill_method,"(",bounds[1],",",bounds[2],")")

   for (i in 1:length(fh)) {
     fh[[i]]<-fh[[i]]+eval(parse(text=scale_fill_method))
     #fh[[i]]<-fh[[i]]+scale_fill_t2m_diff(bounds[1],bounds[2])
     #fh[[i]]<-fh[[i]]+scale_fill_t2m_diff()

   }
  return (fh)
}


p_plot_harmonize <- function(ff,iround,ffname) {

  fmin<-Inf
  fmax<--Inf

  i<-1
  for (fff in ff) {
    if (ffname == "default") {
      fffname <- paste0(fff[[i]]$fcst_model[1],"_mbr000")
    } else { fffname <- ffname }
    
    fmin<-min(fmin,fff[[i]][[fffname]][[1]],na.rm=T)
    fmax<-max(fmax,fff[[i]][[fffname]][[1]],na.rm=T)
  }
  return (c(round(fmin,iround),round(fmax,iround)))

}


p_plot_single <- function(fff,cnfg) {

  #
  #
  #

  pf<-list()

  i<-1
  for (fld in cnfg$flds) {
    for (expe in cnfg$exps) {
      #print(paste0(fff[[i]]$fcst_model[1],"_mbr000"))
      pf[[i]] <- plot(fff[[i]],!!sym(paste0(fff[[i]]$fcst_model[1],"_mbr000")))
      i<-i+1
    }
  }

  return (pf)

}

scale_fill_template <- function(name,colours,direction,limits,breaks,opts="") {
    sfill <- paste0(name,"(colours=scico(64,palette=\"",colours,"\",direction=",direction,",), limits =",limits,",na.value=NA, breaks=",breaks,",oob=scales::censor",opts,")")
    return(sfill)
}



scale_fill_t2ma <- function(min,max) {
  scale_fill_gradient2(
    low="blue",high="red",mid="white",midpoint=0,limits = c(min,max),breaks=c(min,min/2,min/4,-1,0,1,5,max))
}

scale_fill_t2mb <- function(min,max) {
  scale_fill_scico(
    6,palette="vik",midpoint=0,limits = c(min,max),breaks=c(min,min/2,min/4,-1,0,1,max/2,max))
}

scale_fill_t2m <- function(min,max) {
  bnd<-max(abs(c(min,max)))
  scale_fill_stepsn(
    colours=scico(128,palette="vik", direction = 1), limits = c(bnd*-1,-1,1, bnd), na.value = NA,
    breaks = c(seq(bnd*-1,bnd,3)), oob = scales::censor)
}

scale_fill_t2m_warm <- function(min,max) {
  scale_fill_stepsn(
    colours=scico(128,palette="bilbao", direction = -1), limits = c(min, max), na.value = NA,
    breaks = c(seq(min,max,3)), oob = scales::censor)
}

scale_fill_t2m_cold <- function(min,max) {
  scale_fill_stepsn(
    colours=scico(128,palette="oslo", direction = 1), limits = c(-28, 0), na.value = NA,
    breaks = c(seq(-28,0,2)), oob = scales::censor)
}

scale_fill_t2m_diff <- function(min,max) {
  scale_fill_stepsn(
    colours=scico(128,palette="vik", direction = 1), limits = c(-13, 13), na.value = NA,
    breaks = c(seq(-21,21,2)))
}
