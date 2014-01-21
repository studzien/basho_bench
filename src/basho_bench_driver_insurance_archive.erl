-module(basho_bench_driver_insurance_archive).

-compile(export_all).

-import(basho_bench_driver_insurance, [random_string/1]).

-include("include/basho_bench_driver_insurance.hrl").
-include_lib("exml/include/exml.hrl").

run(addNewDocument, _, _, #state{docs=Docs}=State) ->
    S = random_string(10),
    I = random_int(10),
    case send_request(req_add(S, I, S, S, S, I), "AddNewDocument", State) of
        {ok, Response} ->
            Id = exml_query:path(Response, [{element, <<"s:Body">>},
                                            {element, <<"AddNewDocumentResponse">>},
                                            {element, <<"AddNewDocumentResult">>},
                                            cdata]),
            {ok, State#state{docs=[Id|Docs]}};
        {error, Reason} ->
            {error, Reason, State}
    end;
run(createBackup, _, _, State) ->
    Response = send_request(req_backup(random_int(10)), "CreateBackup", State),
    check_response(Response, State);
run(deleteDocument, _, _, #state{docs=[Id|Rest]}=State) ->
    case send_request(req_delete(Id), "DeleteDocument", State) of
        {ok, _}         -> {ok, State#state{docs=Rest}};
        {error, Reason} -> {error, Reason, State}
    end;
run(editDocumentDescription, _, _, #state{docs=[Id|_]}=State) ->
    R = random_string(10),
    Response = send_request(req_edit(Id, R, R, R), "EditDocumentDescription", State),
    check_response(Response, State);
run(getAllDocuments, _, _, State) ->
    check_response(send_request(req_get(), "GetAllDocuments", State), State);
run(_Op, _KeyGen, _ValueGen, State) ->
    {silent, State}.

req_get() ->
    Body = #xmlel{name = <<"arc:GetAllDocuments">>,
                  children = [el(<<"arc:clientId">>, random_int(10))
                  ]},
    envelope(Body).

req_edit(Id, Description, Remarks, Category) ->
    Body = #xmlel{name = <<"arc:EditDocumentDescription">>,
                  children = [el(<<"arc:documentId">>, Id),
                              el(<<"arc:description">>, Description),
                              el(<<"arc:remarks">>, Remarks),
                              el(<<"arc:category">>, Category)
                              ]},
    envelope(Body).

req_add(Category, ClientId, Description, Name, Remarks, WorkerId) ->
    Body = #xmlel{name = <<"arc:AddNewDocument">>,
                  children = [#xmlel{name = <<"arc:details">>,
                                     children = [el(<<"arc1:Category">>, Category),
                                                 el(<<"arc1:ClientId">>, ClientId),
                                                 el(<<"arc1:Description">>, Description),
                                                 el(<<"arc1:Name">>, Name),
                                                 el(<<"arc1:Remarks">>, Remarks),
                                                 el(<<"arc1:WorkerId">>, WorkerId)
                                                 ]}
                              ]},
    envelope(Body).

req_delete(Id) ->
    Body = #xmlel{name = <<"arc:DeleteDocument">>,
                  children = [el(<<"arc:documentId">>, Id)]},
    envelope(Body).

req_backup(ClientId) ->
    Body = #xmlel{name = <<"arc:DownloadBackupRequest">>,
                  children = [el(<<"arc:ClientId">>, ClientId)]},
    envelope(Body).

el(Name, Value) ->
    #xmlel{name = Name, children = #xmlcdata{content = Value}}.

envelope(Body) ->
    #xmlel{name = <<"soapenv:Envelope">>,
           attrs = [{<<"xmlns:soapenv">>,
                     <<"http://schemas.xmlsoap.org/soap/envelope/">>},
                    {<<"xmlns:arc">>,
                     <<"http://archivingdocumentsmodule.com">>},
                    {<<"xmlns:arc1">>,
                     <<"http://schemas.datacontract.org/2004/07/ArchivingDocumentsWcfService">>}],
           children = [#xmlel{name = <<"soapenv:Header">>},
                       #xmlel{name = <<"soapenv:Body">>,
                              children = [Body]}
                      ]}.

send_request(Request, Action, #state{archive=Endpoint}) ->
    case lhttpc:request(Endpoint, "POST",
                        [{"Content-Type", "text/xml;charset=UTF-8"},
                         {"SOAPAction", "http://archivingdocumentsmodule.com/IArchivingService/" ++ Action}],
                        exml:to_binary(Request), 60000) of
        {ok, {{200, "OK"}, _, Response}} ->
            exml:parse(Response);
        {ok, {{_, Status}, _, _}} ->
            {error, Status};
        Other ->
            Other
    end.

get_sid(Response) ->
    exml_query:path(Response,
                    [{element, <<"soap:Body">>},
                     {element, <<"ns1:loginResponse">>},
                     {element, <<"AuthenticationResponse">>}, 
                     {element, <<"sid">>}, 
                     cdata]).

random_int(N) ->
    integer_to_binary(random:uniform(N)).

check_response({ok, _}, State) ->
    {ok, State};
check_response({error, Reason}, State) ->
    {error, Reason, State};
check_response(silent, State) ->
    {silent, State}.
