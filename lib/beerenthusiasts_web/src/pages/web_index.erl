-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
    #template { file="/opt/beerenthusiasts/wwwroot/template.html"}.

title() ->
    "web_index".

body() ->
    [#h3 { text="Upload Profile Image" },
     #p{},
     #upload { tag=upload_image, button_text="Upload Image" }].
	
event(_) -> ok.

upload_event(_Tag, undefined, _) ->
    wf:flash("Please select a file."),
    ok;

upload_event(_Tag, _FileName, LocalFileData) ->
    erls3:put_file(LocalFileData, wf:to_list(wf_utils:pickle(erlang:now())), "public-read"),
    ok.
