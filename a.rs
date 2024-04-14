fn f() {

attrs! ( hello = "world" join = {"me"});
attrs! [ #if true { hello = "world" } else { join = {"me"} } ];
}
