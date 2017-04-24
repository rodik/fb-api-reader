
## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "CET")
}

# param n.posts is not used
get.facebook.page <- function(fb_page_id, n.posts = 100, since = '2016/01/01', until = '2016/02/01', get_comments = T, get_replies = T, oauth_token) {
    
    all_posts <- tibble()
    all_comments <- tibble()
    all_replies <- tibble()
    
    all_posts <- get.posts(fb_page_id, oauth_token, n.posts, since, until)
        
    # convert to tibble
    all_posts <- tibble::as_data_frame(all_posts)
    
    print("finished reading posts")
    
    if(get_comments)
    {
        for (i in 1:nrow(all_posts)) {
            # dohvati trenutni post
            post <- all_posts[i, ]
            
            # za svaki post koji ima komentara pozovi dohvat komentara
            if (length(post) > 0 && post$comments_count > 0) {
                coms <- get.post.comments(post$id, oauth_token)
                if (is.data.frame(coms)) {
                    all_comments <- rbind(all_comments, coms)
                }
                print(paste(i, "/", nrow(all_posts), "post_id:", post$id, "date:",  post$created_time))
            }
            
        }
    }
    
    print("finished reading comments")
    
    if(get_comments && get_replies)
    {
        # za svaki komentar provjeri ima li odgovora i dohvati ako ima
        for (i in 1:nrow(all_comments)) {
            # dohvati trenutni komentar
            comment <- all_comments[i, ]
            
            # za komentar koji ima odgovore pozovi dohvat odgovora
            if (length(comment) > 0 && comment$comments_count > 0) {
                reps <- get.comment.replies(comment$id, oauth_token)
                if (is.data.frame(reps)){
                    all_replies <- rbind(all_replies, reps)
                }
                print(paste(i, "/", nrow(all_comments), "comment_id:", comment$id, "date:", comment$created_time))
            }
        }
    }
    
    print("finished reading replies")
    
    # convert to tibbles and convert created_time column to datetime
    if(nrow(all_replies) > 0) {
        all_replies <- tibble::as_data_frame(all_replies)
        all_replies$created_time <- format.facebook.date(all_replies$created_time)    
    }
    if(nrow(all_comments) > 0) {
        all_comments <- tibble::as_data_frame(all_comments)
        all_comments$created_time <- format.facebook.date(all_comments$created_time)    
    }
    if(nrow(all_posts) > 0) {
        all_posts$created_time <- format.facebook.date(all_posts$created_time)
    }
    
    # return list of data.frames
    list(posts = all_posts, comments = all_comments, replies = all_replies)
}

get.posts <- function(fb_page_id, oauth_token, n.posts, since, until){
    # convert limits to dates
    start_date <- as.Date(since)
    end_date <- as.Date(until)
    
    date <- start_date
    # loop dates
    while(date < end_date){
        
        day_posts <- tryCatch(
            {
                getPage(fb_page_id, oauth_token, n = 200, since = date, until = date + 1, reactions = TRUE)
            },
            error = function(cond){
                if (!grepl(pattern = "No public posts were found", cond)){
                    message(cond) #ignore error        
                } 
            },
            warning = function(cond){
                if (!grepl(pattern = "No public posts were found", cond)){
                    message(cond) #ignore error        
                }
            }
        )
        print(paste("scraping:", as.character(date), "Posts:", nrow(day_posts), sep = " "))
        
        if(exists("posts_df")){
            posts_df <- rbind(posts_df, day_posts)
        } else {
            posts_df <- day_posts
        }
        
        date <- date + 1
    }
    posts_df
}

get.post.comments <- function(post_id, oauth_token) {
    post_comments <- tryCatch({
        getPost(
            post_id,
            oauth_token,
            n = 10000,
            likes = FALSE,
            comments = TRUE,
            reactions = FALSE
        )
    }, error = function(e) {
        print(e)
    }
    )
    
    if(!is.null(post_comments) && is.data.frame(post_comments$comments) && nrow(post_comments$comments) > 0)
        cbind(post_comments$comments, post_id = post_id)
    else
        return()
}

get.comment.replies <- function(comment_id, oauth_token) {
    comment_replies <- tryCatch({
        getCommentReplies(
            comment_id,
            oauth_token,
            n = 1000,
            replies = TRUE,
            likes = FALSE
        )
    }, error = function(e) {
        print(e)
    })
    
    
    if(!is.null(comment_replies) && is.data.frame(comment_replies$replies) && nrow(comment_replies$replies) > 0)
        cbind(comment_replies$replies, comment_id = comment_id)
    else
        return()
}

get.comment.likes <- function(comment_id, oauth_token) {
    comment_likes <- tryCatch({
        getCommentReplies(
            comment_id,
            oauth_token,
            n = 1000,
            replies = FALSE,
            likes = TRUE
        )
    }, error = function(e) {
        print(e)
    })
    
    
    if(!is.null(comment_likes) && is.data.frame(comment_likes$likes) && nrow(comment_likes$likes) > 0)
        cbind(comment_likes$likes, comment_id = comment_id)
    else
        return()
}

get.multiple.comments.likes <- function(comments, oauth_token) {
    
    all_likes <- tibble()
    
    # NULL?, mora biti vektor stringova od kojih je svaki comment_id
    if(!is.null(comments)){
        # za svaki comment_id dohvati njegove lajkove
        for(c_id in comments){
            lks <- get.comment.likes(c_id, oauth_token)
            
            if (is.data.frame(lks))
                all_likes <- rbind(all_likes, lks)
        }
    }
    # return likes
    all_likes
}


save.post.data.csv <- function(data, folder_name = "ForImport") {
    
    if(
        is.list(data) && 
        is.data.frame(data$posts) &&
        nrow(data$posts) > 0
    )
    {
        # get file name (page_id)
        file_name <- as.character(data$posts[1,"from_id"])
        
        # save posts
        write.csv2(
            x = data$posts, 
            file = paste(folder_name,"/",file_name,"_posts",".csv",sep = ""),
            quote = TRUE,
            na="")
        
        # save comments
        write.csv2(
            x = data$comments, 
            file = paste(folder_name,"/",file_name,"_comments",".csv",sep = ""),
            quote = TRUE,
            na="")
        
        # save replies
        write.csv2(
            x = data$replies, 
            file = paste(folder_name,"/",file_name,"_replies",".csv",sep = ""),
            quote = TRUE,
            na="")
    }
    else 
    {
        print("data is empty")
    }
}

save.likes.csv <- function(data, file_name, folder_name = "ForImport") {
    if(
        is.data.frame(data) &&
        nrow(data) > 0
    )
    {
        # save likes
        write.csv2(
            x = data, 
            file = paste(folder_name,"/",file_name,"_comment_likes",".csv",sep = ""),
            quote = TRUE,
            na="")
    }
    else 
    {
        print("data is empty")
    }
}