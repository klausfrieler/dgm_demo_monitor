library(tidyverse)

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

num_predictors <- c("GMS.general", "ART.points", "RAT.ability",  "age")
cat_predictors <- c("gender")


dummy <- list()
dummy[["RAT"]] <- tibble(RAT.ability  = NA, RAT.ability_sem  = NA, RAT.num_items = NA)
dummy[["SRS"]] <- tibble(SRS.perc_correct  = NA, SRS.num_correct = NA, SRS.num_items = NA)
dummy[["ART"]] <- tibble(ART.perc_correct  = NA, ART.num_correct = NA, ART.num_items = NA, ART.points = NA)
dummy[["DEG"]] <- tibble(DEG.age = NA,   DEG.gender = NA)
dummy[["GMS"]] <- tibble(GMS.general = NA)


parse_generic_entry <- function(q_entry, label){
  #browser()
  dummy_entry <- dummy[[label]]
  stopifnot(!is.null(dummy_entry))
  if(is.null(q_entry)){
    return(dummy_entry)
  }
  names <- names(q_entry)
  if(length(names) == 0){
    return(dummy_entry)
  }
  sum_data <- names[!stringr::str_detect(names, "q[0-9]+")]
  ret <- q_entry[sum_data]
  names(ret) <- sprintf("%s.%s", label, names(ret) %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"))
  ret %>% tibble::as_tibble()
}

get_parameters <- function(data, input, keep_pseudo_na = T, var_data){
  
  vars <- c("x" = input$bv_variable1, "y" = input$bv_variable2)
  var_info1 <- var_data %>% filter(variable == input$bv_variable1)
  var_info2 <- var_data %>% filter(variable == input$bv_variable2)
  sub_type <- sprintf("%s-%s", substr(var_info1$type, 1, 3), substr(var_info2$type, 1, 3))  
  list(vars = vars, sub_type = sub_type)
}


split_multi_entry <- function(entry){
  if(length(entry) == 1){
    ret <- str_replace_all(entry, "',", "@") %>% 
      str_replace_all("'", "") %>% 
      str_split("@") %>% 
      unlist()        
  }    
  else{
    ret <- NULL
  }
  ret
}

join_rows <- function(data){
  if(is.null(data[["p_id"]])){
    return(data)
  }
  ids <- data %>% count(p_id) %>% filter(n > 1) %>% pull(p_id)
  ret <- data %>% filter(!(p_id %in% ids))
  fixed_rows <- 
    map_dfr(ids, function(i){
    tmp <- data %>% filter(p_id == i)
    completed <- which(tmp$complete == TRUE)
    if(length(completed) == 0){
      tmp  <- tmp[nrow(tmp),]   
    }
    else{
      tmp <- tmp[max(completed), ]  
    }
    tmp
  })
  ret %>% bind_rows(fixed_rows) 
}

read_data <- function(result_dir = "data/from_server"){
  messagef("Setting up data from %s", result_dir)
  
  results <- purrr::map(list.files(result_dir, pattern = "*.rds", full.names = T), ~{readRDS(.x) %>% as.list()})
  #browser()
  purrr::map_dfr(results, function(x){
    #browser()
    names <- names(x)
    if(length(intersect(names, c("ART", "RAT", "SRS", "GMS", "DEG"))) != 5){
      return(NULL)
    }

    ret <- parse_generic_entry(x$ART, "ART")
    ret <- parse_generic_entry(x$SRS, "SRS") %>% bind_cols(ret)
    RAT <- parse_generic_entry(x$RAT, "RAT")
    attr(RAT$RAT.ability, "metadata") <- NULL
    ret <-  RAT %>% bind_cols(ret)
    ret <- parse_generic_entry(x$GMS, "GMS") %>% bind_cols(ret )
    ret <- parse_generic_entry(x$DEG, "DEG") %>% bind_cols(ret )
    if("DEG.handedness" %in% names(ret)){
      return(NULL)
    }
    
    session_tibble <- x$session %>%  tibble::as_tibble() 
    ret <- ret %>% dplyr::bind_cols(session_tibble) %>% 
      dplyr::select(p_id, -pilot, -num_restarts, time_ended = current_time, everything())
    ret
      
    }) %>% 
    join_rows() %>% 
    dplyr::arrange(time_started)
}
fake_p_id <- function(p_id){
  map_chr(p_id, function(x){
    x %>% str_split("") %>% unlist() %>% sample(size = length(.)) %>% paste(collapse = "")
  })
}
fake_data <- function(data, size = 100L){
  map_dfc(names(data), function(var_name){
    var_type <- class(data[[var_name]]) 
    #browser()
    messagef("Simulating %s with type %s [n = %d]", var_name, var_type[[1]], size)
    if("numeric" %in% var_type ){
      if(var_name != "DEG.gender"){
        m <- mean(data[[var_name]])
        sd <- sd(data[[var_name]])
        if(is.na(sd) || sd == 0 ){
          sd <- runif(1)  
        }
        tmp <- rnorm(size, m, sd) 
      }
      else{
        tmp <- sample(1:4, size = size, replace = T, prob = c(.45, .45, .05, .05))
      }
    }
    else{
      if(var_name == "p_id"){
        tmp <- fake_p_id(rep(data[[var_name]][1], size))
      }
      else{
        tmp <- sample(data[[var_name]], size, replace = T)
      }
    }
    tibble({{var_name}} := tmp)
  }) %>% mutate(complete = TRUE, pilot = FALSE)
  
}

setup_workspace <- function(results = "data/from_server", fake_data = FALSE){
  master <- read_data(results) 
  if(fake_data){
    fd <- fake_data(master, size = 50)
    master <- master %>% bind_rows(fd)
  }
  master <- master %>% mutate(age = round(DEG.age/12), 
                              gender = factor(DEG.gender, 
                                              levels = 1:4, 
                                              labels = c("female", "male", "other", "rather not say"))) 
  master <- master %>% select(-RAT.num_items, 
                              -SRS.num_items, 
                              -ART.num_items, 
                              -SRS.num_correct, 
                              -ART.num_correct, 
                              -ART.num_foils,
                              -ART.num_writers)
  assign("master", master, globalenv())
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())     
}

get_pc_graph <- function(master, 
                         alpha = .05,   
                         charge = -120,
                         linkDistance = 100,
                         fontSize = 16,
                         opacityNoHover = .75){
  #require(Rgraphviz)
  
  red <- master
  cor_data <- cor(red, use = "pairwise.complete.obs")
  #psych::pairs.panels(red)
  suffStat <- list(C = cor_data, n = nrow(red))
  pc.fit <- pcalg::pc(suffStat, indepTest = pcalg::gaussCItest, p = ncol(red), alpha = alpha)
  labels <- names(red)
  names(labels) <- pc.fit@graph@nodes
  nAttrs <- list()
  nAttrs$label <- labels
  attrs <- list(node = list(shape = "ellipse", fixedsize = FALSE))
  
  adjm <- wgtMatrix(getGraph(pc.fit), transpose = FALSE)
  #ig_network <- graph_from_adjacency_matrix(adjm, mode = "directed", weighted = T)
  ig_network <- graph_from_graphnel(getGraph(pc.fit), name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  d3_network <- igraph_to_networkD3(ig_network)
  d3_network$nodes$group <- 1
  d3_network$nodes$name <- labels
  for(i in 1:nrow(d3_network$links)){
    d3_network$links[i,]$value <-  50*abs(cor_data[d3_network$links[i,]$source + 1,  d3_network$links[i,]$target + 1])
  }
  sn <- forceNetwork(
    Links = d3_network$links, 
    Nodes = d3_network$nodes, 
    Source = 'source', 
    Target = 'target', 
    Value = "value",
    NodeID = 'name', 
    Group = "group", 
    fontSize = fontSize,
    opacityNoHover = opacityNoHover,
    bounded = F,
    zoom = T,
    charge = charge,
    linkDistance = linkDistance,
    arrows = TRUE,
    #Nodesize = "size",
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
  )
  #q <- pcalg::iplotPC(pc.fit, labels = labels)
  #plot(pc.fit@graph, main = "", nodeAttrs = nAttrs, attrs = attrs)
  sn
}
beta_plot <- function(lm_model, order_by_size = F){
  if("lm" %in% class(lm_model)){
    lm_model <- lm_model %>% 
      broom::tidy()
  }
  lm_model <- lm_model %>% 
    filter(term != "(Intercept)") %>% 
    mutate(y_min = estimate  - 1.96*std.error, y_max = estimate  + 1.96*std.error, 
           sig = y_min > 0 | y_max < 0)
  
  if(order_by_size) 
    lm_model <- lm_model %>% mutate(term = factor(term) %>% fct_reorder(estimate, mean))
  if("N" %in%  names(lm_model)){
    q <- lm_model %>% 
      mutate(N_fact = factor(N)) %>% 
      ggplot(aes(x = term, y = estimate, colour = sig, group = N_fact)) 
    q <- q + geom_point(shape = 2, size = 2, position = position_dodge(width = 1)) 
    q <- q + geom_linerange(aes(ymin = y_min, ymax = y_max, colour = sig, group = N_fact), position = position_dodge(width = 1))
    q <- q + geom_text(aes(y = 2, x = 10 * (N - min(N))/max(N) + 2, label = sprintf("N = %s", N)), 
                       size = 3, colour ="black")                            
    q <- q + ylim(-1, 1)
  }
  else{
    q <- lm_model %>% ggplot(aes(x = term, y = estimate )) 
    q <- q + geom_point(shape = 2, size = 2, color = def_colour1) 
    q <- q + geom_linerange(aes(ymin = y_min, ymax = y_max))
  }
  q <- q + coord_flip()  
  q <- q + geom_hline(yintercept = 0, linetype = "dashed")
  q <- q + theme(legend.position = "none")
  q
}

get_model <- function(data, dv = "SRS.perc_correct", predictors = num_predictors, output_format = "raw", ...){
  output_format <- match.arg(output_format, c("raw","summary", "glance", "tidy", "sj", "jtools_tab", "jtools_plot"))
  predictors <- setdiff(predictors, dv)
  data <- data %>% select(all_of(c(dv, predictors))) %>%  mutate_if(is.numeric, scale)
  f <- sprintf("%s ~ .", dv) %>% as.formula()                      
  lm_tab <- lm(f, data = data)
  if(output_format == "summary"){
    lm_tab <- lm_tab %>% 
      summary()
  }
  if(output_format == "tidy"){
    lm_tab <- lm_tab %>% 
      broom::tidy()
  }
  if(output_format == "glance"){
    lm_tab <- lm_tab %>% 
      broom::glance()
  }
  if(output_format == "sj"){
    lm_tab <- lm_tab %>% 
      sjPlot::tab_model(...)
  }
  if(output_format == "jtools_tab"){
    lm_tab <- lm_tab %>% 
      jtools::summ(...)
  }
  if(output_format == "jtools_plot"){
    lm_tab <- lm_tab %>% 
      jtools::plot_summs(scale = T,  ...)
  }
  lm_tab
}
