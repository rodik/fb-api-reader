# fb-api-reader
functions for automating Facebook API access

You will have to create a Facebook App and use it to connect to the API. Instructions are [here](http://thinktostart.com/analyzing-facebook-with-r/).

Functions are built using [RFacebook](https://github.com/pablobarbera/Rfacebook) package.

After the initial setup, harvesting data should be a single call:
```r
page_data <- get.facebook.page(
                            page_id, 
                            since = '2017/01/01',
                            until = '2017/04/24',
                            get_comments = T,
                            get_replies = T,
                            oauth_token = fb_oauth
                          )
```                     
