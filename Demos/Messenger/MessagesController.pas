unit MessagesController;

interface

uses
  System.SysUtils, System.DateUtils, FireDAC.Comp.Client, JsonDataObjects,

  SimpleAPI.Controller, SimpleAPI.Utils;

type

  // Controller is a class, which is used to contain actions available via API
  // It can be registered to API through "RegisterControllers"
  // We can set up name for this controller using Controller attribute
  [Controller('messages')]
  TMessagesController = class(TController)
  public
    // This action is available for everyone, without authorization
    // Everyone can read messages from rooms
    // Action can be called from browser by GET request
    // e.g. http://localhost:8080/messages/get?roomId=room&fromTime=0
    [Action, PublicAccess]
    function Get(RoomId: string; FromTime: Int64): string;

    // But this action is not PublicAccess
    // It will be available by TOKEN
    // TOKEN can be passed to URL
    // http://localhost:8080/messages/send/<TOKEN>?roomId=room&text=Hello
    // Read the additional notes from "MainUnit.pas" to learn how to obtain your TOKEN
    // e.g. http://localhost:8080/messages/send/ee231498d6ef59c232906c103a34282c01672478d6cdb7a4efba8975db1af4e5?roomId=room&text=Hello
    [Action]
    function Send(RoomId, Text: string): string;
  end;

implementation

{ TMessagesController }

function TMessagesController.Get(RoomId: string; FromTime: Int64): string;
var
  q: TFDQuery;
  Messages: TJsonArray;
  Answer: TJsonObject;
begin
  // Validate RoomId for length and irregular characters
  // If our RoomId is invalid, then return the error
  if not CheckString([RoomId], [1, 20]) then
    exit('{"error":"invalid_roomid"}');

  // Session object creates for every HTTP request of the action
  // It contains a DB connection and query
  // It has already associated with an authorized user, if "users accounts features" were initialized
  // and the action is not "public access"
  q := SessionObject.FDQuery;

  // Let's make this request to search messages from RoomId room and posted starting from FromTime
  // Note: We should consider the messages with same time and from previous second
  // to fix some problems with requesting "simultaneous" messages
  q.Open(Format('SELECT * FROM messages WHERE roomId = ''%s'' AND time + 1 >= %s ORDER BY time DESC;', [RoomId, FromTime.ToString]));

  // Then we'll push all the messages into Messages json array

  Messages := TJsonArray.Create;

  if q.RecordCount > 0 then
  begin
    q.First;
    while not q.Eof do
    begin
      with Messages.AddObject do
      begin
        S['userId'] := q.FieldValues['userId'];
        U['time'] := q.FieldValues['time'];
        S['message'] := q.FieldValues['message'];
        S['id'] := q.FieldValues['id'];
      end;
      q.Next;
    end;
  end;

  // And here we pack messages into json object to sena it as answer
  Answer := TJsonObject.Create;
  Answer.A['messages'] := Messages;
  result := Answer.ToJSON;
  Answer.DisposeOf;
end;

function TMessagesController.Send(RoomId, Text: string): string;
var
  q: TFDQuery;
  CurrentUserId: string;
  UTCUnixTime: Int64;
  AddedMessageId: string;
begin
  if not CheckString([RoomId], [1, 20]) then
    exit('{"error":"invalid_roomid"}');

  // IMPORTANT! Be careful for SQL injections!

  // As we've said before, SessionObject knows about authorized UserId inside a non-PublicAccess action
  CurrentUserId := SessionObject.UserId;

  // Get UTC unix time
  UTCUnixTime := DateTimeToUnix(Now, false);

  // Do this SQL to add message to messages table
  SessionObject.FDQuery.Open(Format('INSERT INTO messages (time, roomId, userId, message) VALUES (''%s'', ''%s'', ''%s'', ''%s'') RETURNING id;',
    [UTCUnixTime.ToString, RoomId, CurrentUserId, Text]));
  SessionObject.FDQuery.First;

  // We can also return id of our new message
  AddedMessageId := SessionObject.FDQuery.FieldValues['id'];
  result := Format('{"id":"%s"}', [AddedMessageId]);
end;

end.
