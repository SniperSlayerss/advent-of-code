let read_file_as_string filename = 
    let ic = open_in filename in 
    try
        let contents = really_input_string ic (in_channel_length ic) in
        close_in ic;
        contents
    with e ->
        close_in_noerr ic;
        raise e

