{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

-- | TD API function call arguments
module TDLib.Generated.FunArgs where

import Data.ByteString.Base64.Type
import GHC.Generics
import Language.Haskell.Codegen.TH
import Language.TL.I64
import TDLib.Generated.Types


-- | Parameter of Function getAuthorizationState
data GetAuthorizationState
  = -- | Returns the current authorization state; this is an offline request. For informational purposes only. Use updateAuthorizationState instead to maintain the current authorization state
  GetAuthorizationState
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setTdlibParameters
data SetTdlibParameters
  = -- | Sets the parameters for TDLib initialization. Works only when the current authorization state is authorizationStateWaitTdlibParameters 
  SetTdlibParameters
    { -- | Parameters
      parameters :: TdlibParameters
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkDatabaseEncryptionKey
data CheckDatabaseEncryptionKey
  = -- | Checks the database encryption key for correctness. Works only when the current authorization state is authorizationStateWaitEncryptionKey 
  CheckDatabaseEncryptionKey
    { -- | Encryption key to check or set up
      encryption_key :: ByteString64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setAuthenticationPhoneNumber
data SetAuthenticationPhoneNumber
  = -- | Sets the phone number of the user and sends an authentication code to the user. Works only when the current authorization state is authorizationStateWaitPhoneNumber,
  SetAuthenticationPhoneNumber
    { -- | The phone number of the user, in international format 
      phone_number :: T,
      -- | Settings for the authentication of the user's phone number
      settings :: PhoneNumberAuthenticationSettings
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resendAuthenticationCode
data ResendAuthenticationCode
  = -- | Re-sends an authentication code to the user. Works only when the current authorization state is authorizationStateWaitCode and the next_code_type of the result is not null
  ResendAuthenticationCode
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkAuthenticationCode
data CheckAuthenticationCode
  = -- | Checks the authentication code. Works only when the current authorization state is authorizationStateWaitCode 
  CheckAuthenticationCode
    { -- | The verification code received via SMS, Telegram message, phone call, or flash call
      code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function requestQrCodeAuthentication
data RequestQrCodeAuthentication
  = -- | Requests QR code authentication by scanning a QR code on another logged in device. Works only when the current authorization state is authorizationStateWaitPhoneNumber 
  RequestQrCodeAuthentication
    { -- | List of user identifiers of other users currently using the client
      other_user_ids :: [I32]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function registerUser
data RegisterUser
  = -- | Finishes user registration. Works only when the current authorization state is authorizationStateWaitRegistration
  RegisterUser
    { -- | The first name of the user; 1-64 characters 
      first_name :: T,
      -- | The last name of the user; 0-64 characters
      last_name :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkAuthenticationPassword
data CheckAuthenticationPassword
  = -- | Checks the authentication password for correctness. Works only when the current authorization state is authorizationStateWaitPassword 
  CheckAuthenticationPassword
    { -- | The password to check
      password :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function requestAuthenticationPasswordRecovery
data RequestAuthenticationPasswordRecovery
  = -- | Requests to send a password recovery code to an email address that was previously set up. Works only when the current authorization state is authorizationStateWaitPassword
  RequestAuthenticationPasswordRecovery
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function recoverAuthenticationPassword
data RecoverAuthenticationPassword
  = -- | Recovers the password with a password recovery code sent to an email address that was previously set up. Works only when the current authorization state is authorizationStateWaitPassword 
  RecoverAuthenticationPassword
    { -- | Recovery code to check
      recovery_code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkAuthenticationBotToken
data CheckAuthenticationBotToken
  = -- | Checks the authentication token of a bot; to log in as a bot. Works only when the current authorization state is authorizationStateWaitPhoneNumber. Can be used instead of setAuthenticationPhoneNumber and checkAuthenticationCode to log in 
  CheckAuthenticationBotToken
    { -- | The bot token
      token :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function logOut
data LogOut
  = -- | Closes the TDLib instance after a proper logout. Requires an available network connection. All local data will be destroyed. After the logout completes, updateAuthorizationState with authorizationStateClosed will be sent
  LogOut
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function close
data Close
  = -- | Closes the TDLib instance. All databases will be flushed to disk and properly closed. After the close completes, updateAuthorizationState with authorizationStateClosed will be sent
  Close
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function destroy
data Destroy
  = -- | Closes the TDLib instance, destroying all local data without a proper logout. The current user session will remain in the list of all active sessions. All local data will be destroyed. After the destruction completes updateAuthorizationState with authorizationStateClosed will be sent
  Destroy
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function confirmQrCodeAuthentication
data ConfirmQrCodeAuthentication
  = -- | Confirms QR code authentication on another device. Returns created session on success 
  ConfirmQrCodeAuthentication
    { -- | A link from a QR code. The link must be scanned by the in-app camera
      link :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getCurrentState
data GetCurrentState
  = -- | Returns all updates needed to restore current TDLib state, i.e. all actual UpdateAuthorizationState/UpdateUser/UpdateNewChat and others. This is especially useful if TDLib is run in a separate process. This is an offline method. Can be called before authorization
  GetCurrentState
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setDatabaseEncryptionKey
data SetDatabaseEncryptionKey
  = -- | Changes the database encryption key. Usually the encryption key is never changed and is stored in some OS keychain 
  SetDatabaseEncryptionKey
    { -- | New encryption key
      new_encryption_key :: ByteString64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPasswordState
data GetPasswordState
  = -- | Returns the current state of 2-step verification
  GetPasswordState
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setPassword
data SetPassword
  = -- | Changes the password for the user. If a new recovery email address is specified, then the change will not be applied until the new recovery email address is confirmed
  SetPassword
    { -- | Previous password of the user 
      old_password :: T,
      -- | New password of the user; may be empty to remove the password 
      new_password :: T,
      -- | New password hint; may be empty 
      new_hint :: T,
      -- | Pass true if the recovery email address should be changed 
      set_recovery_email_address :: Bool,
      -- | New recovery email address; may be empty
      new_recovery_email_address :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getRecoveryEmailAddress
data GetRecoveryEmailAddress
  = -- | Returns a 2-step verification recovery email address that was previously set up. This method can be used to verify a password provided by the user 
  GetRecoveryEmailAddress
    { -- | The password for the current user
      password :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setRecoveryEmailAddress
data SetRecoveryEmailAddress
  = -- | Changes the 2-step verification recovery email address of the user. If a new recovery email address is specified, then the change will not be applied until the new recovery email address is confirmed.
  SetRecoveryEmailAddress
    { password :: T,
      new_recovery_email_address :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkRecoveryEmailAddressCode
data CheckRecoveryEmailAddressCode
  = -- | Checks the 2-step verification recovery email address verification code 
  CheckRecoveryEmailAddressCode
    { -- | Verification code
      code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resendRecoveryEmailAddressCode
data ResendRecoveryEmailAddressCode
  = -- | Resends the 2-step verification recovery email address verification code
  ResendRecoveryEmailAddressCode
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function requestPasswordRecovery
data RequestPasswordRecovery
  = -- | Requests to send a password recovery code to an email address that was previously set up
  RequestPasswordRecovery
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function recoverPassword
data RecoverPassword
  = -- | Recovers the password using a recovery code sent to an email address that was previously set up 
  RecoverPassword
    { -- | Recovery code to check
      recovery_code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createTemporaryPassword
data CreateTemporaryPassword
  = -- | Creates a new temporary password for processing payments 
  CreateTemporaryPassword
    { -- | Persistent user password 
      password :: T,
      -- | Time during which the temporary password will be valid, in seconds; should be between 60 and 86400
      valid_for :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getTemporaryPasswordState
data GetTemporaryPasswordState
  = -- | Returns information about the current temporary password
  GetTemporaryPasswordState
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getMe
data GetMe
  = -- | Returns the current user
  GetMe
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getUser
data GetUser
  = -- | Returns information about a user by their identifier. This is an offline request if the current user is not a bot 
  GetUser
    { -- | User identifier
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getUserFullInfo
data GetUserFullInfo
  = -- | Returns full information about a user by their identifier 
  GetUserFullInfo
    { -- | User identifier
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getBasicGroup
data GetBasicGroup
  = -- | Returns information about a basic group by its identifier. This is an offline request if the current user is not a bot 
  GetBasicGroup
    { -- | Basic group identifier
      basic_group_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getBasicGroupFullInfo
data GetBasicGroupFullInfo
  = -- | Returns full information about a basic group by its identifier 
  GetBasicGroupFullInfo
    { -- | Basic group identifier
      basic_group_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getSupergroup
data GetSupergroup
  = -- | Returns information about a supergroup or a channel by its identifier. This is an offline request if the current user is not a bot 
  GetSupergroup
    { -- | Supergroup or channel identifier
      supergroup_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getSupergroupFullInfo
data GetSupergroupFullInfo
  = -- | Returns full information about a supergroup or a channel by its identifier, cached for up to 1 minute 
  GetSupergroupFullInfo
    { -- | Supergroup or channel identifier
      supergroup_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getSecretChat
data GetSecretChat
  = -- | Returns information about a secret chat by its identifier. This is an offline request 
  GetSecretChat
    { -- | Secret chat identifier
      secret_chat_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChat
data GetChat
  = -- | Returns information about a chat by its identifier, this is an offline request if the current user is not a bot 
  GetChat
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getMessage
data GetMessage
  = -- | Returns information about a message 
  GetMessage
    { -- | Identifier of the chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message to get
      message_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getMessageLocally
data GetMessageLocally
  = -- | Returns information about a message, if it is available locally without sending network request. This is an offline request 
  GetMessageLocally
    { -- | Identifier of the chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message to get
      message_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getRepliedMessage
data GetRepliedMessage
  = -- | Returns information about a message that is replied by given message 
  GetRepliedMessage
    { -- | Identifier of the chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message reply to which get
      message_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatPinnedMessage
data GetChatPinnedMessage
  = -- | Returns information about a pinned chat message 
  GetChatPinnedMessage
    { -- | Identifier of the chat the message belongs to
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getMessages
data GetMessages
  = -- | Returns information about messages. If a message is not found, returns null on the corresponding position of the result 
  GetMessages
    { -- | Identifier of the chat the messages belong to 
      chat_id :: I53,
      -- | Identifiers of the messages to get
      message_ids :: [I53]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getFile
data GetFile
  = -- | Returns information about a file; this is an offline request 
  GetFile
    { -- | Identifier of the file to get
      file_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getRemoteFile
data GetRemoteFile
  = -- | Returns information about a file by its remote ID; this is an offline request. Can be used to register a URL as a file for further uploading, or sending as a message. Even the request succeeds, the file can be used only if it is still accessible to the user.
  GetRemoteFile
    { -- | Remote identifier of the file to get 
      remote_file_id :: T,
      -- | File type, if known
      file_type :: FileType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChats
data GetChats
  = -- | Returns an ordered list of chats in a chat list. Chats are sorted by the pair (order, chat_id) in decreasing order. (For example, to get a list of chats from the beginning, the offset_order should be equal to a biggest signed 64-bit number 9223372036854775807 == 2^63 - 1).
  GetChats
    { -- | The chat list in which to return chats
      chat_list :: ChatList,
      -- | Chat order to return chats from 
      offset_order :: I64,
      -- | Chat identifier to return chats from
      offset_chat_id :: I53,
      -- | The maximum number of chats to be returned. It is possible that fewer chats than the limit are returned even if the end of the list is not reached
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchPublicChat
data SearchPublicChat
  = -- | Searches a public chat by its username. Currently only private chats, supergroups and channels can be public. Returns the chat if found; otherwise an error is returned 
  SearchPublicChat
    { -- | Username to be resolved
      username :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchPublicChats
data SearchPublicChats
  = -- | Searches public chats by looking for specified query in their username and title. Currently only private chats, supergroups and channels can be public. Returns a meaningful number of results. Returns nothing if the length of the searched username prefix is less than 5. Excludes private chats with contacts and chats from the chat list from the results 
  SearchPublicChats
    { -- | Query to search for
      query :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchChats
data SearchChats
  = -- | Searches for the specified query in the title and username of already known chats, this is an offline request. Returns chats in the order seen in the chat list 
  SearchChats
    { -- | Query to search for. If the query is empty, returns up to 20 recently found chats 
      query :: T,
      -- | The maximum number of chats to be returned
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchChatsOnServer
data SearchChatsOnServer
  = -- | Searches for the specified query in the title and username of already known chats via request to the server. Returns chats in the order seen in the chat list 
  SearchChatsOnServer
    { -- | Query to search for 
      query :: T,
      -- | The maximum number of chats to be returned
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchChatsNearby
data SearchChatsNearby
  = -- | Returns a list of users and location-based supergroups nearby. The list of users nearby will be updated for 60 seconds after the request by the updates updateUsersNearby. The request should be sent again every 25 seconds with adjusted location to not miss new chats 
  SearchChatsNearby
    { -- | Current user location
      location :: Location
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getTopChats
data GetTopChats
  = -- | Returns a list of frequently used chats. Supported only if the chat info database is enabled 
  GetTopChats
    { -- | Category of chats to be returned 
      category :: TopChatCategory,
      -- | The maximum number of chats to be returned; up to 30
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeTopChat
data RemoveTopChat
  = -- | Removes a chat from the list of frequently used chats. Supported only if the chat info database is enabled 
  RemoveTopChat
    { -- | Category of frequently used chats 
      category :: TopChatCategory,
      -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addRecentlyFoundChat
data AddRecentlyFoundChat
  = -- | Adds a chat to the list of recently found chats. The chat is added to the beginning of the list. If the chat is already in the list, it will be removed from the list first 
  AddRecentlyFoundChat
    { -- | Identifier of the chat to add
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeRecentlyFoundChat
data RemoveRecentlyFoundChat
  = -- | Removes a chat from the list of recently found chats 
  RemoveRecentlyFoundChat
    { -- | Identifier of the chat to be removed
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function clearRecentlyFoundChats
data ClearRecentlyFoundChats
  = -- | Clears the list of recently found chats
  ClearRecentlyFoundChats
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkChatUsername
data CheckChatUsername
  = -- | Checks whether a username can be set for a chat 
  CheckChatUsername
    { -- | Chat identifier; should be identifier of a supergroup chat, or a channel chat, or a private chat with self, or zero if chat is being created 
      chat_id :: I53,
      -- | Username to be checked
      username :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getCreatedPublicChats
data GetCreatedPublicChats
  = -- | Returns a list of public chats of the specified type, owned by the user 
  GetCreatedPublicChats
    { -- | Type of the public chats to return
      type_ :: PublicChatType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkCreatedPublicChatsLimit
data CheckCreatedPublicChatsLimit
  = -- | Checks whether the maximum number of owned public chats has been reached. Returns corresponding error if the limit was reached 
  CheckCreatedPublicChatsLimit
    { -- | Type of the public chats, for which to check the limit
      type_ :: PublicChatType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getSuitableDiscussionChats
data GetSuitableDiscussionChats
  = -- | Returns a list of basic group and supergroup chats, which can be used as a discussion group for a channel. Basic group chats need to be first upgraded to supergroups before they can be set as a discussion group
  GetSuitableDiscussionChats
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getInactiveSupergroupChats
data GetInactiveSupergroupChats
  = -- | Returns a list of recently inactive supergroups and channels. Can be used when user reaches limit on the number of joined supergroups and channels and receives CHANNELS_TOO_MUCH error
  GetInactiveSupergroupChats
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getGroupsInCommon
data GetGroupsInCommon
  = -- | Returns a list of common group chats with a given user. Chats are sorted by their type and creation date 
  GetGroupsInCommon
    { -- | User identifier 
      user_id :: I32,
      -- | Chat identifier starting from which to return chats; use 0 for the first request 
      offset_chat_id :: I53,
      -- | The maximum number of chats to be returned; up to 100
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatHistory
data GetChatHistory
  = -- | Returns messages in a chat. The messages are returned in a reverse chronological order (i.e., in order of decreasing message_id).
  GetChatHistory
    { -- | Chat identifier
      chat_id :: I53,
      -- | Identifier of the message starting from which history must be fetched; use 0 to get results from the last message
      from_message_id :: I53,
      -- | Specify 0 to get results from exactly the from_message_id or a negative offset up to 99 to get additionally some newer messages
      offset :: I32,
      -- | The maximum number of messages to be returned; must be positive and can't be greater than 100. If the offset is negative, the limit must be greater or equal to -offset. Fewer messages may be returned than specified by the limit, even if the end of the message history has not been reached
      limit :: I32,
      -- | If true, returns only messages that are available locally without sending network requests
      only_local :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteChatHistory
data DeleteChatHistory
  = -- | Deletes all messages in the chat. Use Chat.can_be_deleted_only_for_self and Chat.can_be_deleted_for_all_users fields to find whether and how the method can be applied to the chat
  DeleteChatHistory
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Pass true if the chat should be removed from the chat list 
      remove_from_chat_list :: Bool,
      -- | Pass true to try to delete chat history for all users
      revoke :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchChatMessages
data SearchChatMessages
  = -- | Searches for messages with given words in the chat. Returns the results in reverse chronological order, i.e. in order of decreasing message_id. Cannot be used in secret chats with a non-empty query
  SearchChatMessages
    { -- | Identifier of the chat in which to search messages
      chat_id :: I53,
      -- | Query to search for
      query :: T,
      -- | If not 0, only messages sent by the specified user will be returned. Not supported in secret chats
      sender_user_id :: I32,
      -- | Identifier of the message starting from which history must be fetched; use 0 to get results from the last message
      from_message_id :: I53,
      -- | Specify 0 to get results from exactly the from_message_id or a negative offset to get the specified message and some newer messages
      offset :: I32,
      -- | The maximum number of messages to be returned; must be positive and can't be greater than 100. If the offset is negative, the limit must be greater than -offset. Fewer messages may be returned than specified by the limit, even if the end of the message history has not been reached
      limit :: I32,
      -- | Filter for message content in the search results
      filter :: SearchMessagesFilter
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchMessages
data SearchMessages
  = -- | Searches for messages in all chats except secret chats. Returns the results in reverse chronological order (i.e., in order of decreasing (date, chat_id, message_id)).
  SearchMessages
    { -- | Chat list in which to search messages; pass null to search in all chats regardless of their chat list
      chat_list :: ChatList,
      -- | Query to search for
      query :: T,
      -- | The date of the message starting from which the results should be fetched. Use 0 or any date in the future to get results from the last message
      offset_date :: I32,
      -- | The chat identifier of the last found message, or 0 for the first request
      offset_chat_id :: I53,
      -- | The message identifier of the last found message, or 0 for the first request
      offset_message_id :: I53,
      -- | The maximum number of messages to be returned, up to 100. Fewer messages may be returned than specified by the limit, even if the end of the message history has not been reached
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchSecretMessages
data SearchSecretMessages
  = -- | Searches for messages in secret chats. Returns the results in reverse chronological order. For optimal performance the number of returned messages is chosen by the library
  SearchSecretMessages
    { -- | Identifier of the chat in which to search. Specify 0 to search in all secret chats 
      chat_id :: I53,
      -- | Query to search for. If empty, searchChatMessages should be used instead
      query :: T,
      -- | The identifier from the result of a previous request, use 0 to get results from the last message
      from_search_id :: I64,
      -- | The maximum number of messages to be returned; up to 100. Fewer messages may be returned than specified by the limit, even if the end of the message history has not been reached
      limit :: I32,
      -- | A filter for the content of messages in the search results
      filter :: SearchMessagesFilter
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchCallMessages
data SearchCallMessages
  = -- | Searches for call messages. Returns the results in reverse chronological order (i. e., in order of decreasing message_id). For optimal performance the number of returned messages is chosen by the library
  SearchCallMessages
    { -- | Identifier of the message from which to search; use 0 to get results from the last message
      from_message_id :: I53,
      -- | The maximum number of messages to be returned; up to 100. Fewer messages may be returned than specified by the limit, even if the end of the message history has not been reached 
      limit :: I32,
      -- | If true, returns only messages with missed calls
      only_missed :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchChatRecentLocationMessages
data SearchChatRecentLocationMessages
  = -- | Returns information about the recent locations of chat members that were sent to the chat. Returns up to 1 location message per user 
  SearchChatRecentLocationMessages
    { -- | Chat identifier 
      chat_id :: I53,
      -- | The maximum number of messages to be returned
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getActiveLiveLocationMessages
data GetActiveLiveLocationMessages
  = -- | Returns all active live locations that should be updated by the client. The list is persistent across application restarts only if the message database is used
  GetActiveLiveLocationMessages
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatMessageByDate
data GetChatMessageByDate
  = -- | Returns the last message sent in a chat no later than the specified date 
  GetChatMessageByDate
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Point in time (Unix timestamp) relative to which to search for messages
      date :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatMessageCount
data GetChatMessageCount
  = -- | Returns approximate number of messages of the specified type in the chat 
  GetChatMessageCount
    { -- | Identifier of the chat in which to count messages 
      chat_id :: I53,
      -- | Filter for message content; searchMessagesFilterEmpty is unsupported in this function 
      filter :: SearchMessagesFilter,
      -- | If true, returns count that is available locally without sending network requests, returning -1 if the number of messages is unknown
      return_local :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatScheduledMessages
data GetChatScheduledMessages
  = -- | Returns all scheduled messages in a chat. The messages are returned in a reverse chronological order (i.e., in order of decreasing message_id) 
  GetChatScheduledMessages
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeNotification
data RemoveNotification
  = -- | Removes an active notification from notification list. Needs to be called only if the notification is removed by the current user 
  RemoveNotification
    { -- | Identifier of notification group to which the notification belongs 
      notification_group_id :: I32,
      -- | Identifier of removed notification
      notification_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeNotificationGroup
data RemoveNotificationGroup
  = -- | Removes a group of active notifications. Needs to be called only if the notification group is removed by the current user 
  RemoveNotificationGroup
    { -- | Notification group identifier 
      notification_group_id :: I32,
      -- | The maximum identifier of removed notifications
      max_notification_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPublicMessageLink
data GetPublicMessageLink
  = -- | Returns a public HTTPS link to a message. Available only for messages in supergroups and channels with a username
  GetPublicMessageLink
    { -- | Identifier of the chat to which the message belongs
      chat_id :: I53,
      -- | Identifier of the message
      message_id :: I53,
      -- | Pass true if a link for a whole media album should be returned
      for_album :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getMessageLink
data GetMessageLink
  = -- | Returns a private HTTPS link to a message in a chat. Available only for already sent messages in supergroups and channels. The link will work only for members of the chat
  GetMessageLink
    { -- | Identifier of the chat to which the message belongs
      chat_id :: I53,
      -- | Identifier of the message
      message_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getMessageLinkInfo
data GetMessageLinkInfo
  = -- | Returns information about a public or private message link 
  GetMessageLinkInfo
    { -- | The message link in the format "https://t.me/c/...", or "tg://privatepost?...", or "https://t.me/username/...", or "tg://resolve?..."
      url :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendMessage
data SendMessage
  = -- | Sends a message. Returns the sent message
  SendMessage
    { -- | Target chat 
      chat_id :: I53,
      -- | Identifier of the message to reply to or 0
      reply_to_message_id :: I53,
      -- | Options to be used to send the message
      options :: SendMessageOptions,
      -- | Markup for replying to the message; for bots only 
      reply_markup :: ReplyMarkup,
      -- | The content of the message to be sent
      input_message_content :: InputMessageContent
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendMessageAlbum
data SendMessageAlbum
  = -- | Sends messages grouped together into an album. Currently only photo and video messages can be grouped into an album. Returns sent messages
  SendMessageAlbum
    { -- | Target chat 
      chat_id :: I53,
      -- | Identifier of a message to reply to or 0
      reply_to_message_id :: I53,
      -- | Options to be used to send the messages
      options :: SendMessageOptions,
      -- | Contents of messages to be sent
      input_message_contents :: [InputMessageContent]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendBotStartMessage
data SendBotStartMessage
  = -- | Invites a bot to a chat (if it is not yet a member) and sends it the /start command. Bots can't be invited to a private chat other than the chat with the bot. Bots can't be invited to channels (although they can be added as admins) and secret chats. Returns the sent message
  SendBotStartMessage
    { -- | Identifier of the bot 
      bot_user_id :: I32,
      -- | Identifier of the target chat 
      chat_id :: I53,
      -- | A hidden parameter sent to the bot for deep linking purposes (https://core.telegram.org/bots#deep-linking)
      parameter :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendInlineQueryResultMessage
data SendInlineQueryResultMessage
  = -- | Sends the result of an inline query as a message. Returns the sent message. Always clears a chat draft message
  SendInlineQueryResultMessage
    { -- | Target chat 
      chat_id :: I53,
      -- | Identifier of a message to reply to or 0
      reply_to_message_id :: I53,
      -- | Options to be used to send the message
      options :: SendMessageOptions,
      -- | Identifier of the inline query 
      query_id :: I64,
      -- | Identifier of the inline result
      result_id :: T,
      -- | If true, there will be no mention of a bot, via which the message is sent. Can be used only for bots GetOption("animation_search_bot_username"), GetOption("photo_search_bot_username") and GetOption("venue_search_bot_username")
      hide_via_bot :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function forwardMessages
data ForwardMessages
  = -- | Forwards previously sent messages. Returns the forwarded messages in the same order as the message identifiers passed in message_ids. If a message can't be forwarded, null will be returned instead of the message
  ForwardMessages
    { -- | Identifier of the chat to which to forward messages 
      chat_id :: I53,
      -- | Identifier of the chat from which to forward messages 
      from_chat_id :: I53,
      -- | Identifiers of the messages to forward
      message_ids :: [I53],
      -- | Options to be used to send the messages
      options :: SendMessageOptions,
      -- | True, if the messages should be grouped into an album after forwarding. For this to work, no more than 10 messages may be forwarded, and all of them must be photo or video messages
      as_album :: Bool,
      -- | True, if content of the messages needs to be copied without links to the original messages. Always true if the messages are forwarded to a secret chat
      send_copy :: Bool,
      -- | True, if media captions of message copies needs to be removed. Ignored if send_copy is false
      remove_caption :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resendMessages
data ResendMessages
  = -- | Resends messages which failed to send. Can be called only for messages for which messageSendingStateFailed.can_retry is true and after specified in messageSendingStateFailed.retry_after time passed.
  ResendMessages
    { -- | Identifier of the chat to send messages 
      chat_id :: I53,
      -- | Identifiers of the messages to resend. Message identifiers must be in a strictly increasing order
      message_ids :: [I53]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendChatSetTtlMessage
data SendChatSetTtlMessage
  = -- | Changes the current TTL setting (sets a new self-destruct timer) in a secret chat and sends the corresponding message 
  SendChatSetTtlMessage
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New TTL value, in seconds
      ttl :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendChatScreenshotTakenNotification
data SendChatScreenshotTakenNotification
  = -- | Sends a notification about a screenshot taken in a chat. Supported only in private and secret chats 
  SendChatScreenshotTakenNotification
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addLocalMessage
data AddLocalMessage
  = -- | Adds a local message to a chat. The message is persistent across application restarts only if the message database is used. Returns the added message 
  AddLocalMessage
    { -- | Target chat 
      chat_id :: I53,
      -- | Identifier of the user who will be shown as the sender of the message; may be 0 for channel posts
      sender_user_id :: I32,
      -- | Identifier of the message to reply to or 0 
      reply_to_message_id :: I53,
      -- | Pass true to disable notification for the message 
      disable_notification :: Bool,
      -- | The content of the message to be added
      input_message_content :: InputMessageContent
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteMessages
data DeleteMessages
  = -- | Deletes messages 
  DeleteMessages
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Identifiers of the messages to be deleted 
      message_ids :: [I53],
      -- | Pass true to try to delete messages for all chat members. Always true for supergroups, channels and secret chats
      revoke :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteChatMessagesFromUser
data DeleteChatMessagesFromUser
  = -- | Deletes all messages sent by the specified user to a chat. Supported only for supergroups; requires can_delete_messages administrator privileges 
  DeleteChatMessagesFromUser
    { -- | Chat identifier 
      chat_id :: I53,
      -- | User identifier
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editMessageText
data EditMessageText
  = -- | Edits the text of a message (or a text of a game message). Returns the edited message after the edit is completed on the server side
  EditMessageText
    { -- | The chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message 
      message_id :: I53,
      -- | The new message reply markup; for bots only 
      reply_markup :: ReplyMarkup,
      -- | New text content of the message. Should be of type InputMessageText
      input_message_content :: InputMessageContent
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editMessageLiveLocation
data EditMessageLiveLocation
  = -- | Edits the message content of a live location. Messages can be edited for a limited period of time specified in the live location. Returns the edited message after the edit is completed on the server side
  EditMessageLiveLocation
    { -- | The chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message 
      message_id :: I53,
      -- | The new message reply markup; for bots only 
      reply_markup :: ReplyMarkup,
      -- | New location content of the message; may be null. Pass null to stop sharing the live location
      location :: Location
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editMessageMedia
data EditMessageMedia
  = -- | Edits the content of a message with an animation, an audio, a document, a photo or a video. The media in the message can't be replaced if the message was set to self-destruct. Media can't be replaced by self-destructing media. Media in an album can be edited only to contain a photo or a video. Returns the edited message after the edit is completed on the server side
  EditMessageMedia
    { -- | The chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message 
      message_id :: I53,
      -- | The new message reply markup; for bots only 
      reply_markup :: ReplyMarkup,
      -- | New content of the message. Must be one of the following types: InputMessageAnimation, InputMessageAudio, InputMessageDocument, InputMessagePhoto or InputMessageVideo
      input_message_content :: InputMessageContent
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editMessageCaption
data EditMessageCaption
  = -- | Edits the message content caption. Returns the edited message after the edit is completed on the server side
  EditMessageCaption
    { -- | The chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message 
      message_id :: I53,
      -- | The new message reply markup; for bots only 
      reply_markup :: ReplyMarkup,
      -- | New message content caption; 0-GetOption("message_caption_length_max") characters
      caption :: FormattedText
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editMessageReplyMarkup
data EditMessageReplyMarkup
  = -- | Edits the message reply markup; for bots only. Returns the edited message after the edit is completed on the server side
  EditMessageReplyMarkup
    { -- | The chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message 
      message_id :: I53,
      -- | The new message reply markup
      reply_markup :: ReplyMarkup
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editInlineMessageText
data EditInlineMessageText
  = -- | Edits the text of an inline text or game message sent via a bot; for bots only 
  EditInlineMessageText
    { -- | Inline message identifier 
      inline_message_id :: T,
      -- | The new message reply markup 
      reply_markup :: ReplyMarkup,
      -- | New text content of the message. Should be of type InputMessageText
      input_message_content :: InputMessageContent
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editInlineMessageLiveLocation
data EditInlineMessageLiveLocation
  = -- | Edits the content of a live location in an inline message sent via a bot; for bots only 
  EditInlineMessageLiveLocation
    { -- | Inline message identifier 
      inline_message_id :: T,
      -- | The new message reply markup 
      reply_markup :: ReplyMarkup,
      -- | New location content of the message; may be null. Pass null to stop sharing the live location
      location :: Location
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editInlineMessageMedia
data EditInlineMessageMedia
  = -- | Edits the content of a message with an animation, an audio, a document, a photo or a video in an inline message sent via a bot; for bots only 
  EditInlineMessageMedia
    { -- | Inline message identifier
      inline_message_id :: T,
      -- | The new message reply markup; for bots only 
      reply_markup :: ReplyMarkup,
      -- | New content of the message. Must be one of the following types: InputMessageAnimation, InputMessageAudio, InputMessageDocument, InputMessagePhoto or InputMessageVideo
      input_message_content :: InputMessageContent
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editInlineMessageCaption
data EditInlineMessageCaption
  = -- | Edits the caption of an inline message sent via a bot; for bots only 
  EditInlineMessageCaption
    { -- | Inline message identifier 
      inline_message_id :: T,
      -- | The new message reply markup 
      reply_markup :: ReplyMarkup,
      -- | New message content caption; 0-GetOption("message_caption_length_max") characters
      caption :: FormattedText
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editInlineMessageReplyMarkup
data EditInlineMessageReplyMarkup
  = -- | Edits the reply markup of an inline message sent via a bot; for bots only 
  EditInlineMessageReplyMarkup
    { -- | Inline message identifier 
      inline_message_id :: T,
      -- | The new message reply markup
      reply_markup :: ReplyMarkup
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editMessageSchedulingState
data EditMessageSchedulingState
  = -- | Edits the time when a scheduled message will be sent. Scheduling state of all messages in the same album or forwarded together with the message will be also changed 
  EditMessageSchedulingState
    { -- | The chat the message belongs to 
      chat_id :: I53,
      -- | Identifier of the message 
      message_id :: I53,
      -- | The new message scheduling state. Pass null to send the message immediately
      scheduling_state :: MessageSchedulingState
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getTextEntities
data GetTextEntities
  = -- | Returns all entities (mentions, hashtags, cashtags, bot commands, bank card numbers, URLs, and email addresses) contained in the text. This is an offline method. Can be called before authorization. Can be called synchronously 
  GetTextEntities
    { -- | The text in which to look for entites
      text :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function parseTextEntities
data ParseTextEntities
  = -- | Parses Bold, Italic, Underline, Strikethrough, Code, Pre, PreCode, TextUrl and MentionName entities contained in the text. This is an offline method. Can be called before authorization. Can be called synchronously 
  ParseTextEntities
    { -- | The text to parse 
      text :: T,
      -- | Text parse mode
      parse_mode :: TextParseMode
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function parseMarkdown
data ParseMarkdown
  = -- | Parses Markdown entities in a human-friendly format, ignoring mark up errors. This is an offline method. Can be called before authorization. Can be called synchronously
  ParseMarkdown
    { -- | The text to parse. For example, "__italic__ ~~strikethrough~~ **bold** `code` ```pre``` __[italic__ text_url](telegram.org) __italic**bold italic__bold**"
      text :: FormattedText
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getMarkdownText
data GetMarkdownText
  = -- | Replaces text entities with Markdown formatting in a human-friendly format. Entities that can't be represented in Markdown unambiguously are kept as is. This is an offline method. Can be called before authorization. Can be called synchronously 
  GetMarkdownText
    { -- | The text
      text :: FormattedText
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getFileMimeType
data GetFileMimeType
  = -- | Returns the MIME type of a file, guessed by its extension. Returns an empty string on failure. This is an offline method. Can be called before authorization. Can be called synchronously 
  GetFileMimeType
    { -- | The name of the file or path to the file
      file_name :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getFileExtension
data GetFileExtension
  = -- | Returns the extension of a file, guessed by its MIME type. Returns an empty string on failure. This is an offline method. Can be called before authorization. Can be called synchronously 
  GetFileExtension
    { -- | The MIME type of the file
      mime_type :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function cleanFileName
data CleanFileName
  = -- | Removes potentially dangerous characters from the name of a file. The encoding of the file name is supposed to be UTF-8. Returns an empty string on failure. This is an offline method. Can be called before authorization. Can be called synchronously 
  CleanFileName
    { -- | File name or path to the file
      file_name :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLanguagePackString
data GetLanguagePackString
  = -- | Returns a string stored in the local database from the specified localization target and language pack by its key. Returns a 404 error if the string is not found. This is an offline method. Can be called before authorization. Can be called synchronously
  GetLanguagePackString
    { -- | Path to the language pack database in which strings are stored 
      language_pack_database_path :: T,
      -- | Localization target to which the language pack belongs 
      localization_target :: T,
      -- | Language pack identifier 
      language_pack_id :: T,
      -- | Language pack key of the string to be returned
      key :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getJsonValue
data GetJsonValue
  = -- | Converts a JSON-serialized string to corresponding JsonValue object. This is an offline method. Can be called before authorization. Can be called synchronously 
  GetJsonValue
    { -- | The JSON-serialized string
      json :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getJsonString
data GetJsonString
  = -- | Converts a JsonValue object to corresponding JSON-serialized string. This is an offline method. Can be called before authorization. Can be called synchronously 
  GetJsonString
    { -- | The JsonValue object
      json_value :: JsonValue
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setPollAnswer
data SetPollAnswer
  = -- | Changes the user answer to a poll. A poll in quiz mode can be answered only once
  SetPollAnswer
    { -- | Identifier of the chat to which the poll belongs 
      chat_id :: I53,
      -- | Identifier of the message containing the poll
      message_id :: I53,
      -- | 0-based identifiers of answer options, chosen by the user. User can choose more than 1 answer option only is the poll allows multiple answers
      option_ids :: [I32]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPollVoters
data GetPollVoters
  = -- | Returns users voted for the specified option in a non-anonymous polls. For the optimal performance the number of returned users is chosen by the library
  GetPollVoters
    { -- | Identifier of the chat to which the poll belongs 
      chat_id :: I53,
      -- | Identifier of the message containing the poll
      message_id :: I53,
      -- | 0-based identifier of the answer option
      option_id :: I32,
      -- | Number of users to skip in the result; must be non-negative
      offset :: I32,
      -- | The maximum number of users to be returned; must be positive and can't be greater than 50. Fewer users may be returned than specified by the limit, even if the end of the voter list has not been reached
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function stopPoll
data StopPoll
  = -- | Stops a poll. A poll in a message can be stopped when the message has can_be_edited flag set
  StopPoll
    { -- | Identifier of the chat to which the poll belongs 
      chat_id :: I53,
      -- | Identifier of the message containing the poll 
      message_id :: I53,
      -- | The new message reply markup; for bots only
      reply_markup :: ReplyMarkup
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLoginUrlInfo
data GetLoginUrlInfo
  = -- | Returns information about a button of type inlineKeyboardButtonTypeLoginUrl. The method needs to be called when the user presses the button
  GetLoginUrlInfo
    { -- | Chat identifier of the message with the button 
      chat_id :: I53,
      -- | Message identifier of the message with the button 
      message_id :: I53,
      -- | Button identifier
      button_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLoginUrl
data GetLoginUrl
  = -- | Returns an HTTP URL which can be used to automatically authorize the user on a website after clicking an inline button of type inlineKeyboardButtonTypeLoginUrl.
  GetLoginUrl
    { -- | Chat identifier of the message with the button 
      chat_id :: I53,
      -- | Message identifier of the message with the button 
      message_id :: I53,
      -- | Button identifier
      button_id :: I32,
      -- | True, if the user allowed the bot to send them messages
      allow_write_access :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getInlineQueryResults
data GetInlineQueryResults
  = -- | Sends an inline query to a bot and returns its results. Returns an error with code 502 if the bot fails to answer the query before the query timeout expires 
  GetInlineQueryResults
    { -- | The identifier of the target bot
      bot_user_id :: I32,
      -- | Identifier of the chat where the query was sent 
      chat_id :: I53,
      -- | Location of the user, only if needed 
      user_location :: Location,
      -- | Text of the query 
      query :: T,
      -- | Offset of the first entry to return
      offset :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function answerInlineQuery
data AnswerInlineQuery
  = -- | Sets the result of an inline query; for bots only 
  AnswerInlineQuery
    { -- | Identifier of the inline query 
      inline_query_id :: I64,
      -- | True, if the result of the query can be cached for the specified user
      is_personal :: Bool,
      -- | The results of the query 
      results :: [InputInlineQueryResult],
      -- | Allowed time to cache the results of the query, in seconds 
      cache_time :: I32,
      -- | Offset for the next inline query; pass an empty string if there are no more results
      next_offset :: T,
      -- | If non-empty, this text should be shown on the button that opens a private chat with the bot and sends a start message to the bot with the parameter switch_pm_parameter 
      switch_pm_text :: T,
      -- | The parameter for the bot start message
      switch_pm_parameter :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getCallbackQueryAnswer
data GetCallbackQueryAnswer
  = -- | Sends a callback query to a bot and returns an answer. Returns an error with code 502 if the bot fails to answer the query before the query timeout expires 
  GetCallbackQueryAnswer
    { -- | Identifier of the chat with the message 
      chat_id :: I53,
      -- | Identifier of the message from which the query originated 
      message_id :: I53,
      -- | Query payload
      payload :: CallbackQueryPayload
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function answerCallbackQuery
data AnswerCallbackQuery
  = -- | Sets the result of a callback query; for bots only 
  AnswerCallbackQuery
    { -- | Identifier of the callback query 
      callback_query_id :: I64,
      -- | Text of the answer 
      text :: T,
      -- | If true, an alert should be shown to the user instead of a toast notification 
      show_alert :: Bool,
      -- | URL to be opened 
      url :: T,
      -- | Time during which the result of the query can be cached, in seconds
      cache_time :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function answerShippingQuery
data AnswerShippingQuery
  = -- | Sets the result of a shipping query; for bots only 
  AnswerShippingQuery
    { -- | Identifier of the shipping query 
      shipping_query_id :: I64,
      -- | Available shipping options 
      shipping_options :: [ShippingOption],
      -- | An error message, empty on success
      error_message :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function answerPreCheckoutQuery
data AnswerPreCheckoutQuery
  = -- | Sets the result of a pre-checkout query; for bots only 
  AnswerPreCheckoutQuery
    { -- | Identifier of the pre-checkout query 
      pre_checkout_query_id :: I64,
      -- | An error message, empty on success
      error_message :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setGameScore
data SetGameScore
  = -- | Updates the game score of the specified user in the game; for bots only 
  SetGameScore
    { -- | The chat to which the message with the game belongs 
      chat_id :: I53,
      -- | Identifier of the message 
      message_id :: I53,
      -- | True, if the message should be edited 
      edit_message :: Bool,
      -- | User identifier 
      user_id :: I32,
      -- | The new score
      score :: I32,
      -- | Pass true to update the score even if it decreases. If the score is 0, the user will be deleted from the high score table
      force :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setInlineGameScore
data SetInlineGameScore
  = -- | Updates the game score of the specified user in a game; for bots only 
  SetInlineGameScore
    { -- | Inline message identifier 
      inline_message_id :: T,
      -- | True, if the message should be edited 
      edit_message :: Bool,
      -- | User identifier 
      user_id :: I32,
      -- | The new score
      score :: I32,
      -- | Pass true to update the score even if it decreases. If the score is 0, the user will be deleted from the high score table
      force :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getGameHighScores
data GetGameHighScores
  = -- | Returns the high scores for a game and some part of the high score table in the range of the specified user; for bots only 
  GetGameHighScores
    { -- | The chat that contains the message with the game 
      chat_id :: I53,
      -- | Identifier of the message 
      message_id :: I53,
      -- | User identifier
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getInlineGameHighScores
data GetInlineGameHighScores
  = -- | Returns game high scores and some part of the high score table in the range of the specified user; for bots only 
  GetInlineGameHighScores
    { -- | Inline message identifier 
      inline_message_id :: T,
      -- | User identifier
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteChatReplyMarkup
data DeleteChatReplyMarkup
  = -- | Deletes the default reply markup from a chat. Must be called after a one-time keyboard or a ForceReply reply markup has been used. UpdateChatReplyMarkup will be sent if the reply markup will be changed 
  DeleteChatReplyMarkup
    { -- | Chat identifier
      chat_id :: I53,
      -- | The message identifier of the used keyboard
      message_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendChatAction
data SendChatAction
  = -- | Sends a notification about user activity in a chat 
  SendChatAction
    { -- | Chat identifier 
      chat_id :: I53,
      -- | The action description
      action :: ChatAction
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function openChat
data OpenChat
  = -- | Informs TDLib that the chat is opened by the user. Many useful activities depend on the chat being opened or closed (e.g., in supergroups and channels all updates are received only for opened chats) 
  OpenChat
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function closeChat
data CloseChat
  = -- | Informs TDLib that the chat is closed by the user. Many useful activities depend on the chat being opened or closed 
  CloseChat
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function viewMessages
data ViewMessages
  = -- | Informs TDLib that messages are being viewed by the user. Many useful activities depend on whether the messages are currently being viewed or not (e.g., marking messages as read, incrementing a view counter, updating a view counter, removing deleted messages in supergroups and channels) 
  ViewMessages
    { -- | Chat identifier 
      chat_id :: I53,
      -- | The identifiers of the messages being viewed
      message_ids :: [I53],
      -- | True, if messages in closed chats should be marked as read
      force_read :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function openMessageContent
data OpenMessageContent
  = -- | Informs TDLib that the message content has been opened (e.g., the user has opened a photo, video, document, location or venue, or has listened to an audio file or voice note message). An updateMessageContentOpened update will be generated if something has changed 
  OpenMessageContent
    { -- | Chat identifier of the message 
      chat_id :: I53,
      -- | Identifier of the message with the opened content
      message_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function readAllChatMentions
data ReadAllChatMentions
  = -- | Marks all mentions in a chat as read 
  ReadAllChatMentions
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createPrivateChat
data CreatePrivateChat
  = -- | Returns an existing chat corresponding to a given user 
  CreatePrivateChat
    { -- | User identifier 
      user_id :: I32,
      -- | If true, the chat will be created without network request. In this case all information about the chat except its type, title and photo can be incorrect
      force :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createBasicGroupChat
data CreateBasicGroupChat
  = -- | Returns an existing chat corresponding to a known basic group 
  CreateBasicGroupChat
    { -- | Basic group identifier 
      basic_group_id :: I32,
      -- | If true, the chat will be created without network request. In this case all information about the chat except its type, title and photo can be incorrect
      force :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createSupergroupChat
data CreateSupergroupChat
  = -- | Returns an existing chat corresponding to a known supergroup or channel 
  CreateSupergroupChat
    { -- | Supergroup or channel identifier 
      supergroup_id :: I32,
      -- | If true, the chat will be created without network request. In this case all information about the chat except its type, title and photo can be incorrect
      force :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createSecretChat
data CreateSecretChat
  = -- | Returns an existing chat corresponding to a known secret chat 
  CreateSecretChat
    { -- | Secret chat identifier
      secret_chat_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createNewBasicGroupChat
data CreateNewBasicGroupChat
  = -- | Creates a new basic group and sends a corresponding messageBasicGroupChatCreate. Returns the newly created chat 
  CreateNewBasicGroupChat
    { -- | Identifiers of users to be added to the basic group 
      user_ids :: [I32],
      -- | Title of the new basic group; 1-128 characters
      title :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createNewSupergroupChat
data CreateNewSupergroupChat
  = -- | Creates a new supergroup or channel and sends a corresponding messageSupergroupChatCreate. Returns the newly created chat 
  CreateNewSupergroupChat
    { -- | Title of the new chat; 1-128 characters 
      title :: T,
      -- | True, if a channel chat should be created 
      is_channel :: Bool,
      -- | Creates a new supergroup or channel and sends a corresponding messageSupergroupChatCreate. Returns the newly created chat 
      description :: T,
      -- | Chat location if a location-based supergroup is being created
      location :: ChatLocation
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createNewSecretChat
data CreateNewSecretChat
  = -- | Creates a new secret chat. Returns the newly created chat 
  CreateNewSecretChat
    { -- | Identifier of the target user
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function upgradeBasicGroupChatToSupergroupChat
data UpgradeBasicGroupChatToSupergroupChat
  = -- | Creates a new supergroup from an existing basic group and sends a corresponding messageChatUpgradeTo and messageChatUpgradeFrom; requires creator privileges. Deactivates the original basic group 
  UpgradeBasicGroupChatToSupergroupChat
    { -- | Identifier of the chat to upgrade
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatChatList
data SetChatChatList
  = -- | Moves a chat to a different chat list. Current chat list of the chat must ne non-null 
  SetChatChatList
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New chat list of the chat. The chat with the current user (Saved Messages) and the chat 777000 (Telegram) can't be moved to the Archive chat list
      chat_list :: ChatList
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatTitle
data SetChatTitle
  = -- | Changes the chat title. Supported only for basic groups, supergroups and channels. Requires can_change_info rights. The title will not be changed until the request to the server has been completed
  SetChatTitle
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New title of the chat; 1-128 characters
      title :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatPhoto
data SetChatPhoto
  = -- | Changes the photo of a chat. Supported only for basic groups, supergroups and channels. Requires can_change_info rights. The photo will not be changed before request to the server has been completed
  SetChatPhoto
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New chat photo. You can use a zero InputFileId to delete the chat photo. Files that are accessible only by HTTP URL are not acceptable
      photo :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatPermissions
data SetChatPermissions
  = -- | Changes the chat members permissions. Supported only for basic groups and supergroups. Requires can_restrict_members administrator right
  SetChatPermissions
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New non-administrator members permissions in the chat
      permissions :: ChatPermissions
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatDraftMessage
data SetChatDraftMessage
  = -- | Changes the draft message in a chat 
  SetChatDraftMessage
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New draft message; may be null
      draft_message :: DraftMessage
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatNotificationSettings
data SetChatNotificationSettings
  = -- | Changes the notification settings of a chat. Notification settings of a chat with the current user (Saved Messages) can't be changed
  SetChatNotificationSettings
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New notification settings for the chat. If the chat is muted for more than 1 week, it is considered to be muted forever
      notification_settings :: ChatNotificationSettings
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function toggleChatIsPinned
data ToggleChatIsPinned
  = -- | Changes the pinned state of a chat. You can pin up to GetOption("pinned_chat_count_max")/GetOption("pinned_archived_chat_count_max") non-secret chats and the same number of secret chats in the main/archive chat list 
  ToggleChatIsPinned
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New value of is_pinned
      is_pinned :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function toggleChatIsMarkedAsUnread
data ToggleChatIsMarkedAsUnread
  = -- | Changes the marked as unread state of a chat 
  ToggleChatIsMarkedAsUnread
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New value of is_marked_as_unread
      is_marked_as_unread :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function toggleChatDefaultDisableNotification
data ToggleChatDefaultDisableNotification
  = -- | Changes the value of the default disable_notification parameter, used when a message is sent to a chat 
  ToggleChatDefaultDisableNotification
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New value of default_disable_notification
      default_disable_notification :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatClientData
data SetChatClientData
  = -- | Changes client data associated with a chat 
  SetChatClientData
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New value of client_data
      client_data :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatDescription
data SetChatDescription
  = -- | Changes information about a chat. Available for basic groups, supergroups, and channels. Requires can_change_info rights 
  SetChatDescription
    { -- | Identifier of the chat 
      chat_id :: I53,
      -- | Changes information about a chat. Available for basic groups, supergroups, and channels. Requires can_change_info rights 
      description :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatDiscussionGroup
data SetChatDiscussionGroup
  = -- | Changes the discussion group of a channel chat; requires can_change_info rights in the channel if it is specified 
  SetChatDiscussionGroup
    { -- | Identifier of the channel chat. Pass 0 to remove a link from the supergroup passed in the second argument to a linked channel chat (requires can_pin_messages rights in the supergroup) 
      chat_id :: I53,
      -- | Identifier of a new channel's discussion group. Use 0 to remove the discussion group.
      discussion_chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatLocation
data SetChatLocation
  = -- | Changes the location of a chat. Available only for some location-based supergroups, use supergroupFullInfo.can_set_location to check whether the method is allowed to use 
  SetChatLocation
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New location for the chat; must be valid and not null
      location :: ChatLocation
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatSlowModeDelay
data SetChatSlowModeDelay
  = -- | Changes the slow mode delay of a chat. Available only for supergroups; requires can_restrict_members rights 
  SetChatSlowModeDelay
    { -- | Chat identifier 
      chat_id :: I53,
      -- | New slow mode delay for the chat; must be one of 0, 10, 30, 60, 300, 900, 3600
      slow_mode_delay :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function pinChatMessage
data PinChatMessage
  = -- | Pins a message in a chat; requires can_pin_messages rights 
  PinChatMessage
    { -- | Identifier of the chat 
      chat_id :: I53,
      -- | Identifier of the new pinned message 
      message_id :: I53,
      -- | True, if there should be no notification about the pinned message
      disable_notification :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function unpinChatMessage
data UnpinChatMessage
  = -- | Removes the pinned message from a chat; requires can_pin_messages rights in the group or channel 
  UnpinChatMessage
    { -- | Identifier of the chat
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function joinChat
data JoinChat
  = -- | Adds current user as a new member to a chat. Private and secret chats can't be joined using this method 
  JoinChat
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function leaveChat
data LeaveChat
  = -- | Removes current user from chat members. Private and secret chats can't be left using this method 
  LeaveChat
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addChatMember
data AddChatMember
  = -- | Adds a new member to a chat. Members can't be added to private or secret chats. Members will not be added until the chat state has been synchronized with the server
  AddChatMember
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Identifier of the user 
      user_id :: I32,
      -- | The number of earlier messages from the chat to be forwarded to the new member; up to 100. Ignored for supergroups and channels
      forward_limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addChatMembers
data AddChatMembers
  = -- | Adds multiple new members to a chat. Currently this option is only available for supergroups and channels. This option can't be used to join a chat. Members can't be added to a channel if it has more than 200 members. Members will not be added until the chat state has been synchronized with the server
  AddChatMembers
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Identifiers of the users to be added to the chat
      user_ids :: [I32]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setChatMemberStatus
data SetChatMemberStatus
  = -- | Changes the status of a chat member, needs appropriate privileges. This function is currently not suitable for adding new members to the chat and transferring chat ownership; instead, use addChatMember or transferChatOwnership. The chat member status will not be changed until it has been synchronized with the server
  SetChatMemberStatus
    { -- | Chat identifier 
      chat_id :: I53,
      -- | User identifier 
      user_id :: I32,
      -- | The new status of the member in the chat
      status :: ChatMemberStatus
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function canTransferOwnership
data CanTransferOwnership
  = -- | Checks whether the current session can be used to transfer a chat ownership to another user
  CanTransferOwnership
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function transferChatOwnership
data TransferChatOwnership
  = -- | Changes the owner of a chat. The current user must be a current owner of the chat. Use the method canTransferOwnership to check whether the ownership can be transferred from the current session. Available only for supergroups and channel chats
  TransferChatOwnership
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Identifier of the user to which transfer the ownership. The ownership can't be transferred to a bot or to a deleted user 
      user_id :: I32,
      -- | The password of the current user
      password :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatMember
data GetChatMember
  = -- | Returns information about a single member of a chat 
  GetChatMember
    { -- | Chat identifier 
      chat_id :: I53,
      -- | User identifier
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchChatMembers
data SearchChatMembers
  = -- | Searches for a specified query in the first name, last name and username of the members of a specified chat. Requires administrator rights in channels 
  SearchChatMembers
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Query to search for 
      query :: T,
      -- | The maximum number of users to be returned 
      limit :: I32,
      -- | The type of users to return. By default, chatMembersFilterMembers
      filter :: ChatMembersFilter
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatAdministrators
data GetChatAdministrators
  = -- | Returns a list of administrators of the chat with their custom titles 
  GetChatAdministrators
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function clearAllDraftMessages
data ClearAllDraftMessages
  = -- | Clears draft messages in all chats 
  ClearAllDraftMessages
    { -- | If true, local draft messages in secret chats will not be cleared
      exclude_secret_chats :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatNotificationSettingsExceptions
data GetChatNotificationSettingsExceptions
  = -- | Returns list of chats with non-default notification settings 
  GetChatNotificationSettingsExceptions
    { -- | If specified, only chats from the specified scope will be returned 
      scope :: NotificationSettingsScope,
      -- | If true, also chats with non-default sound will be returned
      compare_sound :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getScopeNotificationSettings
data GetScopeNotificationSettings
  = -- | Returns the notification settings for chats of a given type 
  GetScopeNotificationSettings
    { -- | Types of chats for which to return the notification settings information
      scope :: NotificationSettingsScope
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setScopeNotificationSettings
data SetScopeNotificationSettings
  = -- | Changes notification settings for chats of a given type 
  SetScopeNotificationSettings
    { -- | Types of chats for which to change the notification settings 
      scope :: NotificationSettingsScope,
      -- | The new notification settings for the given scope
      notification_settings :: ScopeNotificationSettings
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resetAllNotificationSettings
data ResetAllNotificationSettings
  = -- | Resets all notification settings to their default values. By default, all chats are unmuted, the sound is set to "default" and message previews are shown
  ResetAllNotificationSettings
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setPinnedChats
data SetPinnedChats
  = -- | Changes the order of pinned chats 
  SetPinnedChats
    { -- | Chat list in which to change the order of pinned chats 
      chat_list :: ChatList,
      -- | The new list of pinned chats
      chat_ids :: [I53]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function downloadFile
data DownloadFile
  = -- | Downloads a file from the cloud. Download progress and completion of the download will be notified through updateFile updates
  DownloadFile
    { -- | Identifier of the file to download
      file_id :: I32,
      -- | Priority of the download (1-32). The higher the priority, the earlier the file will be downloaded. If the priorities of two files are equal, then the last one for which downloadFile was called will be downloaded first
      priority :: I32,
      -- | The starting position from which the file should be downloaded
      offset :: I32,
      -- | Number of bytes which should be downloaded starting from the "offset" position before the download will be automatically cancelled; use 0 to download without a limit
      limit :: I32,
      -- | If false, this request returns file state just after the download has been started. If true, this request returns file state only after
      synchronous :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getFileDownloadedPrefixSize
data GetFileDownloadedPrefixSize
  = -- | Returns file downloaded prefix size from a given offset 
  GetFileDownloadedPrefixSize
    { -- | Identifier of the file 
      file_id :: I32,
      -- | Offset from which downloaded prefix size should be calculated
      offset :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function cancelDownloadFile
data CancelDownloadFile
  = -- | Stops the downloading of a file. If a file has already been downloaded, does nothing 
  CancelDownloadFile
    { -- | Identifier of a file to stop downloading 
      file_id :: I32,
      -- | Pass true to stop downloading only if it hasn't been started, i.e. request hasn't been sent to server
      only_if_pending :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function uploadFile
data UploadFile
  = -- | Asynchronously uploads a file to the cloud without sending it in a message. updateFile will be used to notify about upload progress and successful completion of the upload. The file will not have a persistent remote identifier until it will be sent in a message 
  UploadFile
    { -- | File to upload 
      file :: InputFile,
      -- | File type
      file_type :: FileType,
      -- | Priority of the upload (1-32). The higher the priority, the earlier the file will be uploaded. If the priorities of two files are equal, then the first one for which uploadFile was called will be uploaded first
      priority :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function cancelUploadFile
data CancelUploadFile
  = -- | Stops the uploading of a file. Supported only for files uploaded by using uploadFile. For other files the behavior is undefined 
  CancelUploadFile
    { -- | Identifier of the file to stop uploading
      file_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function writeGeneratedFilePart
data WriteGeneratedFilePart
  = -- | Writes a part of a generated file. This method is intended to be used only if the client has no direct access to TDLib's file system, because it is usually slower than a direct write to the destination file
  WriteGeneratedFilePart
    { -- | The identifier of the generation process 
      generation_id :: I64,
      -- | The offset from which to write the data to the file 
      offset :: I32,
      -- | The data to write
      data_ :: ByteString64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setFileGenerationProgress
data SetFileGenerationProgress
  = -- | Informs TDLib on a file generation progress
  SetFileGenerationProgress
    { -- | The identifier of the generation process
      generation_id :: I64,
      -- | Expected size of the generated file, in bytes; 0 if unknown
      expected_size :: I32,
      -- | The number of bytes already generated
      local_prefix_size :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function finishFileGeneration
data FinishFileGeneration
  = -- | Finishes the file generation
  FinishFileGeneration
    { -- | The identifier of the generation process
      generation_id :: I64,
      -- | If set, means that file generation has failed and should be terminated
      error :: Error
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function readFilePart
data ReadFilePart
  = -- | Reads a part of a file from the TDLib file cache and returns read bytes. This method is intended to be used only if the client has no direct access to TDLib's file system, because it is usually slower than a direct read from the file
  ReadFilePart
    { -- | Identifier of the file. The file must be located in the TDLib file cache
      file_id :: I32,
      -- | The offset from which to read the file
      offset :: I32,
      -- | Number of bytes to read. An error will be returned if there are not enough bytes available in the file from the specified position. Pass 0 to read all available data from the specified position
      count :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteFile
data DeleteFile
  = -- | Deletes a file from the TDLib file cache 
  DeleteFile
    { -- | Identifier of the file to delete
      file_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function generateChatInviteLink
data GenerateChatInviteLink
  = -- | Generates a new invite link for a chat; the previously generated link is revoked. Available for basic groups, supergroups, and channels. Requires administrator privileges and can_invite_users right 
  GenerateChatInviteLink
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkChatInviteLink
data CheckChatInviteLink
  = -- | Checks the validity of an invite link for a chat and returns information about the corresponding chat 
  CheckChatInviteLink
    { -- | Invite link to be checked; should begin with "https://t.me/joinchat/", "https://telegram.me/joinchat/", or "https://telegram.dog/joinchat/"
      invite_link :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function joinChatByInviteLink
data JoinChatByInviteLink
  = -- | Uses an invite link to add the current user to the chat if possible. The new member will not be added until the chat state has been synchronized with the server
  JoinChatByInviteLink
    { -- | Invite link to import; should begin with "https://t.me/joinchat/", "https://telegram.me/joinchat/", or "https://telegram.dog/joinchat/"
      invite_link :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createCall
data CreateCall
  = -- | Creates a new call 
  CreateCall
    { -- | Identifier of the user to be called 
      user_id :: I32,
      -- | Description of the call protocols supported by the client
      protocol :: CallProtocol
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function acceptCall
data AcceptCall
  = -- | Accepts an incoming call 
  AcceptCall
    { -- | Call identifier 
      call_id :: I32,
      -- | Description of the call protocols supported by the client
      protocol :: CallProtocol
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function discardCall
data DiscardCall
  = -- | Discards a call 
  DiscardCall
    { -- | Call identifier 
      call_id :: I32,
      -- | True, if the user was disconnected 
      is_disconnected :: Bool,
      -- | The call duration, in seconds 
      duration :: I32,
      -- | Identifier of the connection used during the call
      connection_id :: I64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendCallRating
data SendCallRating
  = -- | Sends a call rating 
  SendCallRating
    { -- | Call identifier 
      call_id :: I32,
      -- | Call rating; 1-5 
      rating :: I32,
      -- | An optional user comment if the rating is less than 5 
      comment :: T,
      -- | List of the exact types of problems with the call, specified by the user
      problems :: [CallProblem]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendCallDebugInformation
data SendCallDebugInformation
  = -- | Sends debug information for a call 
  SendCallDebugInformation
    { -- | Call identifier 
      call_id :: I32,
      -- | Debug information in application-specific format
      debug_information :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function blockUser
data BlockUser
  = -- | Adds a user to the blacklist 
  BlockUser
    { -- | User identifier
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function unblockUser
data UnblockUser
  = -- | Removes a user from the blacklist 
  UnblockUser
    { -- | User identifier
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getBlockedUsers
data GetBlockedUsers
  = -- | Returns users that were blocked by the current user 
  GetBlockedUsers
    { -- | Number of users to skip in the result; must be non-negative 
      offset :: I32,
      -- | The maximum number of users to return; up to 100
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addContact
data AddContact
  = -- | Adds a user to the contact list or edits an existing contact by their user identifier 
  AddContact
    { -- | The contact to add or edit; phone number can be empty and needs to be specified only if known, vCard is ignored
      contact :: Contact,
      -- | True, if the new contact needs to be allowed to see current user's phone number. A corresponding rule to userPrivacySettingShowPhoneNumber will be added if needed. Use the field UserFullInfo.need_phone_number_privacy_exception to check whether the current user needs to be asked to share their phone number
      share_phone_number :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function importContacts
data ImportContacts
  = -- | Adds new contacts or edits existing contacts by their phone numbers; contacts' user identifiers are ignored 
  ImportContacts
    { -- | The list of contacts to import or edit; contacts' vCard are ignored and are not imported
      contacts :: [Contact]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getContacts
data GetContacts
  = -- | Returns all user contacts
  GetContacts
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchContacts
data SearchContacts
  = -- | Searches for the specified query in the first names, last names and usernames of the known user contacts 
  SearchContacts
    { -- | Query to search for; may be empty to return all contacts 
      query :: T,
      -- | The maximum number of users to be returned
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeContacts
data RemoveContacts
  = -- | Removes users from the contact list 
  RemoveContacts
    { -- | Identifiers of users to be deleted
      user_ids :: [I32]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getImportedContactCount
data GetImportedContactCount
  = -- | Returns the total number of imported contacts
  GetImportedContactCount
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function changeImportedContacts
data ChangeImportedContacts
  = -- | Changes imported contacts using the list of current user contacts saved on the device. Imports newly added contacts and, if at least the file database is enabled, deletes recently deleted contacts.
  ChangeImportedContacts
    { contacts :: [Contact]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function clearImportedContacts
data ClearImportedContacts
  = -- | Clears all imported contacts, contact list remains unchanged
  ClearImportedContacts
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sharePhoneNumber
data SharePhoneNumber
  = -- | Shares the phone number of the current user with a mutual contact. Supposed to be called when the user clicks on chatActionBarSharePhoneNumber 
  SharePhoneNumber
    { -- | Identifier of the user with whom to share the phone number. The user must be a mutual contact
      user_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getUserProfilePhotos
data GetUserProfilePhotos
  = -- | Returns the profile photos of a user. The result of this query may be outdated: some photos might have been deleted already 
  GetUserProfilePhotos
    { -- | User identifier 
      user_id :: I32,
      -- | The number of photos to skip; must be non-negative 
      offset :: I32,
      -- | The maximum number of photos to be returned; up to 100
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getStickers
data GetStickers
  = -- | Returns stickers from the installed sticker sets that correspond to a given emoji. If the emoji is not empty, favorite and recently used stickers may also be returned 
  GetStickers
    { -- | String representation of emoji. If empty, returns all known installed stickers 
      emoji :: T,
      -- | The maximum number of stickers to be returned
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchStickers
data SearchStickers
  = -- | Searches for stickers from public sticker sets that correspond to a given emoji 
  SearchStickers
    { -- | String representation of emoji; must be non-empty 
      emoji :: T,
      -- | The maximum number of stickers to be returned
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getInstalledStickerSets
data GetInstalledStickerSets
  = -- | Returns a list of installed sticker sets 
  GetInstalledStickerSets
    { -- | Pass true to return mask sticker sets; pass false to return ordinary sticker sets
      is_masks :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getArchivedStickerSets
data GetArchivedStickerSets
  = -- | Returns a list of archived sticker sets 
  GetArchivedStickerSets
    { -- | Pass true to return mask stickers sets; pass false to return ordinary sticker sets 
      is_masks :: Bool,
      -- | Identifier of the sticker set from which to return the result 
      offset_sticker_set_id :: I64,
      -- | The maximum number of sticker sets to return
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getTrendingStickerSets
data GetTrendingStickerSets
  = -- | Returns a list of trending sticker sets. For the optimal performance the number of returned sticker sets is chosen by the library
  GetTrendingStickerSets
    { -- | The offset from which to return the sticker sets; must be non-negative
      offset :: I32,
      -- | The maximum number of sticker sets to be returned; must be non-negative. Fewer sticker sets may be returned than specified by the limit, even if the end of the list has not been reached
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getAttachedStickerSets
data GetAttachedStickerSets
  = -- | Returns a list of sticker sets attached to a file. Currently only photos and videos can have attached sticker sets 
  GetAttachedStickerSets
    { -- | File identifier
      file_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getStickerSet
data GetStickerSet
  = -- | Returns information about a sticker set by its identifier 
  GetStickerSet
    { -- | Identifier of the sticker set
      set_id :: I64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchStickerSet
data SearchStickerSet
  = -- | Searches for a sticker set by its name 
  SearchStickerSet
    { -- | Name of the sticker set
      name :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchInstalledStickerSets
data SearchInstalledStickerSets
  = -- | Searches for installed sticker sets by looking for specified query in their title and name 
  SearchInstalledStickerSets
    { -- | Pass true to return mask sticker sets; pass false to return ordinary sticker sets 
      is_masks :: Bool,
      -- | Query to search for 
      query :: T,
      -- | The maximum number of sticker sets to return
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchStickerSets
data SearchStickerSets
  = -- | Searches for ordinary sticker sets by looking for specified query in their title and name. Excludes installed sticker sets from the results 
  SearchStickerSets
    { -- | Query to search for
      query :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function changeStickerSet
data ChangeStickerSet
  = -- | Installs/uninstalls or activates/archives a sticker set 
  ChangeStickerSet
    { -- | Identifier of the sticker set 
      set_id :: I64,
      -- | The new value of is_installed 
      is_installed :: Bool,
      -- | The new value of is_archived. A sticker set can't be installed and archived simultaneously
      is_archived :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function viewTrendingStickerSets
data ViewTrendingStickerSets
  = -- | Informs the server that some trending sticker sets have been viewed by the user 
  ViewTrendingStickerSets
    { -- | Identifiers of viewed trending sticker sets
      sticker_set_ids :: [I64]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function reorderInstalledStickerSets
data ReorderInstalledStickerSets
  = -- | Changes the order of installed sticker sets 
  ReorderInstalledStickerSets
    { -- | Pass true to change the order of mask sticker sets; pass false to change the order of ordinary sticker sets 
      is_masks :: Bool,
      -- | Identifiers of installed sticker sets in the new correct order
      sticker_set_ids :: [I64]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getRecentStickers
data GetRecentStickers
  = -- | Returns a list of recently used stickers 
  GetRecentStickers
    { -- | Pass true to return stickers and masks that were recently attached to photos or video files; pass false to return recently sent stickers
      is_attached :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addRecentSticker
data AddRecentSticker
  = -- | Manually adds a new sticker to the list of recently used stickers. The new sticker is added to the top of the list. If the sticker was already in the list, it is removed from the list first. Only stickers belonging to a sticker set can be added to this list
  AddRecentSticker
    { -- | Pass true to add the sticker to the list of stickers recently attached to photo or video files; pass false to add the sticker to the list of recently sent stickers 
      is_attached :: Bool,
      -- | Sticker file to add
      sticker :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeRecentSticker
data RemoveRecentSticker
  = -- | Removes a sticker from the list of recently used stickers 
  RemoveRecentSticker
    { -- | Pass true to remove the sticker from the list of stickers recently attached to photo or video files; pass false to remove the sticker from the list of recently sent stickers 
      is_attached :: Bool,
      -- | Sticker file to delete
      sticker :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function clearRecentStickers
data ClearRecentStickers
  = -- | Clears the list of recently used stickers 
  ClearRecentStickers
    { -- | Pass true to clear the list of stickers recently attached to photo or video files; pass false to clear the list of recently sent stickers
      is_attached :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getFavoriteStickers
data GetFavoriteStickers
  = -- | Returns favorite stickers
  GetFavoriteStickers
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addFavoriteSticker
data AddFavoriteSticker
  = -- | Adds a new sticker to the list of favorite stickers. The new sticker is added to the top of the list. If the sticker was already in the list, it is removed from the list first. Only stickers belonging to a sticker set can be added to this list
  AddFavoriteSticker
    { -- | Sticker file to add
      sticker :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeFavoriteSticker
data RemoveFavoriteSticker
  = -- | Removes a sticker from the list of favorite stickers 
  RemoveFavoriteSticker
    { -- | Sticker file to delete from the list
      sticker :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getStickerEmojis
data GetStickerEmojis
  = -- | Returns emoji corresponding to a sticker. The list is only for informational purposes, because a sticker is always sent with a fixed emoji from the corresponding Sticker object 
  GetStickerEmojis
    { -- | Sticker file identifier
      sticker :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchEmojis
data SearchEmojis
  = -- | Searches for emojis by keywords. Supported only if the file database is enabled 
  SearchEmojis
    { -- | Text to search for 
      text :: T,
      -- | True, if only emojis, which exactly match text needs to be returned 
      exact_match :: Bool,
      -- | List of possible IETF language tags of the user's input language; may be empty if unknown
      input_language_codes :: [T]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getEmojiSuggestionsUrl
data GetEmojiSuggestionsUrl
  = -- | Returns an HTTP URL which can be used to automatically log in to the translation platform and suggest new emoji replacements. The URL will be valid for 30 seconds after generation 
  GetEmojiSuggestionsUrl
    { -- | Language code for which the emoji replacements will be suggested
      language_code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getSavedAnimations
data GetSavedAnimations
  = -- | Returns saved animations
  GetSavedAnimations
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addSavedAnimation
data AddSavedAnimation
  = -- | Manually adds a new animation to the list of saved animations. The new animation is added to the beginning of the list. If the animation was already in the list, it is removed first. Only non-secret video animations with MIME type "video/mp4" can be added to the list
  AddSavedAnimation
    { -- | The animation file to be added. Only animations known to the server (i.e. successfully sent via a message) can be added to the list
      animation :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeSavedAnimation
data RemoveSavedAnimation
  = -- | Removes an animation from the list of saved animations 
  RemoveSavedAnimation
    { -- | Animation file to be removed
      animation :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getRecentInlineBots
data GetRecentInlineBots
  = -- | Returns up to 20 recently used inline bots in the order of their last usage
  GetRecentInlineBots
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchHashtags
data SearchHashtags
  = -- | Searches for recently used hashtags by their prefix 
  SearchHashtags
    { -- | Hashtag prefix to search for 
      prefix :: T,
      -- | The maximum number of hashtags to be returned
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeRecentHashtag
data RemoveRecentHashtag
  = -- | Removes a hashtag from the list of recently used hashtags 
  RemoveRecentHashtag
    { -- | Hashtag to delete
      hashtag :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getWebPagePreview
data GetWebPagePreview
  = -- | Returns a web page preview by the text of the message. Do not call this function too often. Returns a 404 error if the web page has no preview 
  GetWebPagePreview
    { -- | Message text with formatting
      text :: FormattedText
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getWebPageInstantView
data GetWebPageInstantView
  = -- | Returns an instant view version of a web page if available. Returns a 404 error if the web page has no instant view page 
  GetWebPageInstantView
    { -- | The web page URL 
      url :: T,
      -- | If true, the full instant view for the web page will be returned
      force_full :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setProfilePhoto
data SetProfilePhoto
  = -- | Uploads a new profile photo for the current user. If something changes, updateUser will be sent 
  SetProfilePhoto
    { -- | Profile photo to set. inputFileId and inputFileRemote may still be unsupported
      photo :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteProfilePhoto
data DeleteProfilePhoto
  = -- | Deletes a profile photo. If something changes, updateUser will be sent 
  DeleteProfilePhoto
    { -- | Identifier of the profile photo to delete
      profile_photo_id :: I64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setName
data SetName
  = -- | Changes the first and last name of the current user. If something changes, updateUser will be sent 
  SetName
    { -- | The new value of the first name for the user; 1-64 characters 
      first_name :: T,
      -- | The new value of the optional last name for the user; 0-64 characters
      last_name :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setBio
data SetBio
  = -- | Changes the bio of the current user 
  SetBio
    { -- | The new value of the user bio; 0-70 characters without line feeds
      bio :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setUsername
data SetUsername
  = -- | Changes the username of the current user. If something changes, updateUser will be sent 
  SetUsername
    { -- | The new value of the username. Use an empty string to remove the username
      username :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setLocation
data SetLocation
  = -- | Changes the location of the current user. Needs to be called if GetOption("is_location_visible") is true and location changes for more than 1 kilometer 
  SetLocation
    { -- | The new location of the user
      location :: Location
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function changePhoneNumber
data ChangePhoneNumber
  = -- | Changes the phone number of the user and sends an authentication code to the user's new phone number. On success, returns information about the sent code
  ChangePhoneNumber
    { -- | The new phone number of the user in international format 
      phone_number :: T,
      -- | Settings for the authentication of the user's phone number
      settings :: PhoneNumberAuthenticationSettings
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resendChangePhoneNumberCode
data ResendChangePhoneNumberCode
  = -- | Re-sends the authentication code sent to confirm a new phone number for the user. Works only if the previously received authenticationCodeInfo next_code_type was not null
  ResendChangePhoneNumberCode
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkChangePhoneNumberCode
data CheckChangePhoneNumberCode
  = -- | Checks the authentication code sent to confirm a new phone number of the user 
  CheckChangePhoneNumberCode
    { -- | Verification code received by SMS, phone call or flash call
      code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setCommands
data SetCommands
  = -- | Sets the list of commands supported by the bot; for bots only 
  SetCommands
    { -- | List of the bot's commands
      commands :: [BotCommand]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getActiveSessions
data GetActiveSessions
  = -- | Returns all active sessions of the current user
  GetActiveSessions
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function terminateSession
data TerminateSession
  = -- | Terminates a session of the current user 
  TerminateSession
    { -- | Session identifier
      session_id :: I64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function terminateAllOtherSessions
data TerminateAllOtherSessions
  = -- | Terminates all other sessions of the current user
  TerminateAllOtherSessions
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getConnectedWebsites
data GetConnectedWebsites
  = -- | Returns all website where the current user used Telegram to log in
  GetConnectedWebsites
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function disconnectWebsite
data DisconnectWebsite
  = -- | Disconnects website from the current user's Telegram account 
  DisconnectWebsite
    { -- | Website identifier
      website_id :: I64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function disconnectAllWebsites
data DisconnectAllWebsites
  = -- | Disconnects all websites from the current user's Telegram account
  DisconnectAllWebsites
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setSupergroupUsername
data SetSupergroupUsername
  = -- | Changes the username of a supergroup or channel, requires owner privileges in the supergroup or channel 
  SetSupergroupUsername
    { -- | Identifier of the supergroup or channel 
      supergroup_id :: I32,
      -- | New value of the username. Use an empty string to remove the username
      username :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setSupergroupStickerSet
data SetSupergroupStickerSet
  = -- | Changes the sticker set of a supergroup; requires can_change_info rights 
  SetSupergroupStickerSet
    { -- | Identifier of the supergroup 
      supergroup_id :: I32,
      -- | New value of the supergroup sticker set identifier. Use 0 to remove the supergroup sticker set
      sticker_set_id :: I64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function toggleSupergroupSignMessages
data ToggleSupergroupSignMessages
  = -- | Toggles sender signatures messages sent in a channel; requires can_change_info rights 
  ToggleSupergroupSignMessages
    { -- | Identifier of the channel 
      supergroup_id :: I32,
      -- | New value of sign_messages
      sign_messages :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function toggleSupergroupIsAllHistoryAvailable
data ToggleSupergroupIsAllHistoryAvailable
  = -- | Toggles whether the message history of a supergroup is available to new members; requires can_change_info rights 
  ToggleSupergroupIsAllHistoryAvailable
    { -- | The identifier of the supergroup 
      supergroup_id :: I32,
      -- | The new value of is_all_history_available
      is_all_history_available :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function reportSupergroupSpam
data ReportSupergroupSpam
  = -- | Reports some messages from a user in a supergroup as spam; requires administrator rights in the supergroup 
  ReportSupergroupSpam
    { -- | Supergroup identifier 
      supergroup_id :: I32,
      -- | User identifier 
      user_id :: I32,
      -- | Identifiers of messages sent in the supergroup by the user. This list must be non-empty
      message_ids :: [I53]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getSupergroupMembers
data GetSupergroupMembers
  = -- | Returns information about members or banned users in a supergroup or channel. Can be used only if SupergroupFullInfo.can_get_members == true; additionally, administrator privileges may be required for some filters 
  GetSupergroupMembers
    { -- | Identifier of the supergroup or channel
      supergroup_id :: I32,
      -- | The type of users to return. By default, supergroupMembersRecent 
      filter :: SupergroupMembersFilter,
      -- | Number of users to skip 
      offset :: I32,
      -- | The maximum number of users be returned; up to 200
      limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteSupergroup
data DeleteSupergroup
  = -- | Deletes a supergroup or channel along with all messages in the corresponding chat. This will release the supergroup or channel username and remove all members; requires owner privileges in the supergroup or channel. Chats with more than 1000 members can't be deleted using this method 
  DeleteSupergroup
    { -- | Identifier of the supergroup or channel
      supergroup_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function closeSecretChat
data CloseSecretChat
  = -- | Closes a secret chat, effectively transferring its state to secretChatStateClosed 
  CloseSecretChat
    { -- | Secret chat identifier
      secret_chat_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatEventLog
data GetChatEventLog
  = -- | Returns a list of service actions taken by chat members and administrators in the last 48 hours. Available only for supergroups and channels. Requires administrator rights. Returns results in reverse chronological order (i. e., in order of decreasing event_id)
  GetChatEventLog
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Search query by which to filter events 
      query :: T,
      -- | Identifier of an event from which to return results. Use 0 to get results from the latest events 
      from_event_id :: I64,
      -- | The maximum number of events to return; up to 100
      limit :: I32,
      -- | The types of events to return. By default, all types will be returned 
      filters :: ChatEventLogFilters,
      -- | User identifiers by which to filter events. By default, events relating to all users will be returned
      user_ids :: [I32]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPaymentForm
data GetPaymentForm
  = -- | Returns an invoice payment form. This method should be called when the user presses inlineKeyboardButtonBuy 
  GetPaymentForm
    { -- | Chat identifier of the Invoice message 
      chat_id :: I53,
      -- | Message identifier
      message_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function validateOrderInfo
data ValidateOrderInfo
  = -- | Validates the order information provided by a user and returns the available shipping options for a flexible invoice 
  ValidateOrderInfo
    { -- | Chat identifier of the Invoice message 
      chat_id :: I53,
      -- | Message identifier 
      message_id :: I53,
      -- | The order information, provided by the user 
      order_info :: OrderInfo,
      -- | True, if the order information can be saved
      allow_save :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendPaymentForm
data SendPaymentForm
  = -- | Sends a filled-out payment form to the bot for final verification 
  SendPaymentForm
    { -- | Chat identifier of the Invoice message 
      chat_id :: I53,
      -- | Message identifier 
      message_id :: I53,
      -- | Identifier returned by ValidateOrderInfo, or an empty string 
      order_info_id :: T,
      -- | Identifier of a chosen shipping option, if applicable
      shipping_option_id :: T,
      -- | The credentials chosen by user for payment
      credentials :: InputCredentials
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPaymentReceipt
data GetPaymentReceipt
  = -- | Returns information about a successful payment 
  GetPaymentReceipt
    { -- | Chat identifier of the PaymentSuccessful message 
      chat_id :: I53,
      -- | Message identifier
      message_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getSavedOrderInfo
data GetSavedOrderInfo
  = -- | Returns saved order info, if any
  GetSavedOrderInfo
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteSavedOrderInfo
data DeleteSavedOrderInfo
  = -- | Deletes saved order info
  DeleteSavedOrderInfo
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteSavedCredentials
data DeleteSavedCredentials
  = -- | Deletes saved credentials for all payment provider bots
  DeleteSavedCredentials
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getSupportUser
data GetSupportUser
  = -- | Returns a user that can be contacted to get support
  GetSupportUser
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getBackgrounds
data GetBackgrounds
  = -- | Returns backgrounds installed by the user 
  GetBackgrounds
    { -- | True, if the backgrounds needs to be ordered for dark theme
      for_dark_theme :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getBackgroundUrl
data GetBackgroundUrl
  = -- | Constructs a persistent HTTP URL for a background 
  GetBackgroundUrl
    { -- | Background name 
      name :: T,
      -- | Background type
      type_ :: BackgroundType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function searchBackground
data SearchBackground
  = -- | Searches for a background by its name 
  SearchBackground
    { -- | The name of the background
      name :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setBackground
data SetBackground
  = -- | Changes the background selected by the user; adds background to the list of installed backgrounds
  SetBackground
    { -- | The input background to use, null for filled backgrounds
      background :: InputBackground,
      -- | Background type; null for default background. The method will return error 404 if type is null
      type_ :: BackgroundType,
      -- | True, if the background is chosen for dark theme
      for_dark_theme :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeBackground
data RemoveBackground
  = -- | Removes background from the list of installed backgrounds 
  RemoveBackground
    { -- | The background identifier
      background_id :: I64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resetBackgrounds
data ResetBackgrounds
  = -- | Resets list of installed backgrounds to its default value
  ResetBackgrounds
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLocalizationTargetInfo
data GetLocalizationTargetInfo
  = -- | Returns information about the current localization target. This is an offline request if only_local is true. Can be called before authorization 
  GetLocalizationTargetInfo
    { -- | If true, returns only locally available information without sending network requests
      only_local :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLanguagePackInfo
data GetLanguagePackInfo
  = -- | Returns information about a language pack. Returned language pack identifier may be different from a provided one. Can be called before authorization 
  GetLanguagePackInfo
    { -- | Language pack identifier
      language_pack_id :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLanguagePackStrings
data GetLanguagePackStrings
  = -- | Returns strings from a language pack in the current localization target by their keys. Can be called before authorization 
  GetLanguagePackStrings
    { -- | Language pack identifier of the strings to be returned 
      language_pack_id :: T,
      -- | Language pack keys of the strings to be returned; leave empty to request all available strings
      keys :: [T]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function synchronizeLanguagePack
data SynchronizeLanguagePack
  = -- | Fetches the latest versions of all strings from a language pack in the current localization target from the server. This method doesn't need to be called explicitly for the current used/base language packs. Can be called before authorization 
  SynchronizeLanguagePack
    { -- | Language pack identifier
      language_pack_id :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addCustomServerLanguagePack
data AddCustomServerLanguagePack
  = -- | Adds a custom server language pack to the list of installed language packs in current localization target. Can be called before authorization 
  AddCustomServerLanguagePack
    { -- | Identifier of a language pack to be added; may be different from a name that is used in an "https://t.me/setlanguage/" link
      language_pack_id :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setCustomLanguagePack
data SetCustomLanguagePack
  = -- | Adds or changes a custom local language pack to the current localization target 
  SetCustomLanguagePack
    { -- | Information about the language pack. Language pack ID must start with 'X', consist only of English letters, digits and hyphens, and must not exceed 64 characters. Can be called before authorization 
      info :: LanguagePackInfo,
      -- | Strings of the new language pack
      strings :: [LanguagePackString]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editCustomLanguagePackInfo
data EditCustomLanguagePackInfo
  = -- | Edits information about a custom local language pack in the current localization target. Can be called before authorization 
  EditCustomLanguagePackInfo
    { -- | New information about the custom local language pack
      info :: LanguagePackInfo
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setCustomLanguagePackString
data SetCustomLanguagePackString
  = -- | Adds, edits or deletes a string in a custom local language pack. Can be called before authorization 
  SetCustomLanguagePackString
    { -- | Identifier of a previously added custom local language pack in the current localization target 
      language_pack_id :: T,
      -- | New language pack string
      new_string :: LanguagePackString
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteLanguagePack
data DeleteLanguagePack
  = -- | Deletes all information about a language pack in the current localization target. The language pack which is currently in use (including base language pack) or is being synchronized can't be deleted. Can be called before authorization 
  DeleteLanguagePack
    { -- | Identifier of the language pack to delete
      language_pack_id :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function registerDevice
data RegisterDevice
  = -- | Registers the currently used device for receiving push notifications. Returns a globally unique identifier of the push notification subscription 
  RegisterDevice
    { -- | Device token 
      device_token :: DeviceToken,
      -- | List of user identifiers of other users currently using the client
      other_user_ids :: [I32]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function processPushNotification
data ProcessPushNotification
  = -- | Handles a push notification. Returns error with code 406 if the push notification is not supported and connection to the server is required to fetch new data. Can be called before authorization
  ProcessPushNotification
    { -- | JSON-encoded push notification payload with all fields sent by the server, and "google.sent_time" and "google.notification.sound" fields added
      payload :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPushReceiverId
data GetPushReceiverId
  = -- | Returns a globally unique push notification subscription identifier for identification of an account, which has received a push notification. This is an offline method. Can be called before authorization. Can be called synchronously 
  GetPushReceiverId
    { -- | JSON-encoded push notification payload
      payload :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getRecentlyVisitedTMeUrls
data GetRecentlyVisitedTMeUrls
  = -- | Returns t.me URLs recently visited by a newly registered user 
  GetRecentlyVisitedTMeUrls
    { -- | Google Play referrer to identify the user
      referrer :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setUserPrivacySettingRules
data SetUserPrivacySettingRules
  = -- | Changes user privacy settings 
  SetUserPrivacySettingRules
    { -- | The privacy setting 
      setting :: UserPrivacySetting,
      -- | The new privacy rules
      rules :: UserPrivacySettingRules
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getUserPrivacySettingRules
data GetUserPrivacySettingRules
  = -- | Returns the current privacy settings 
  GetUserPrivacySettingRules
    { -- | The privacy setting
      setting :: UserPrivacySetting
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getOption
data GetOption
  = -- | Returns the value of an option by its name. (Check the list of available options on https://core.telegram.org/tdlib/options.) Can be called before authorization
  GetOption
    { -- | The name of the option
      name :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setOption
data SetOption
  = -- | Sets the value of an option. (Check the list of available options on https://core.telegram.org/tdlib/options.) Only writable options can be set. Can be called before authorization
  SetOption
    { -- | The name of the option 
      name :: T,
      -- | The new value of the option
      value :: OptionValue
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setAccountTtl
data SetAccountTtl
  = -- | Changes the period of inactivity after which the account of the current user will automatically be deleted 
  SetAccountTtl
    { -- | New account TTL
      ttl :: AccountTtl
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getAccountTtl
data GetAccountTtl
  = -- | Returns the period of inactivity after which the account of the current user will automatically be deleted
  GetAccountTtl
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deleteAccount
data DeleteAccount
  = -- | Deletes the account of the current user, deleting all information associated with the user from the server. The phone number of the account can be used to create a new account. Can be called before authorization when the current authorization state is authorizationStateWaitPassword 
  DeleteAccount
    { -- | The reason why the account was deleted; optional
      reason :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeChatActionBar
data RemoveChatActionBar
  = -- | Removes a chat action bar without any other action 
  RemoveChatActionBar
    { -- | Chat identifier
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function reportChat
data ReportChat
  = -- | Reports a chat to the Telegram moderators. A chat can be reported only from the chat action bar, or if this is a private chats with a bot, a private chat with a user sharing their location, a supergroup, or a channel, since other chats can't be checked by moderators 
  ReportChat
    { -- | Chat identifier 
      chat_id :: I53,
      -- | The reason for reporting the chat 
      reason :: ChatReportReason,
      -- | Identifiers of reported messages, if any
      message_ids :: [I53]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatStatisticsUrl
data GetChatStatisticsUrl
  = -- | Returns an HTTP URL with the chat statistics. Currently this method of getting the statistics is disabled and can be deleted in the future 
  GetChatStatisticsUrl
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Parameters from "tg://statsrefresh?params=******" link 
      parameters :: T,
      -- | Pass true if a URL with the dark theme must be returned
      is_dark :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatStatistics
data GetChatStatistics
  = -- | Returns detailed statistics about a chat. Currently this method can be used only for channels. Requires administrator rights in the channel 
  GetChatStatistics
    { -- | Chat identifier 
      chat_id :: I53,
      -- | Pass true if a dark theme is used by the app
      is_dark :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getChatStatisticsGraph
data GetChatStatisticsGraph
  = -- | Loads asynchronous or zoomed in chat statistics graph 
  GetChatStatisticsGraph
    { -- | Chat identifier 
      chat_id :: I53,
      -- | The token for graph loading 
      token :: T,
      -- | X-value for zoomed in graph or 0 otherwise
      x :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getStorageStatistics
data GetStorageStatistics
  = -- | Returns storage usage statistics. Can be called before authorization 
  GetStorageStatistics
    { -- | The maximum number of chats with the largest storage usage for which separate statistics should be returned. All other chats will be grouped in entries with chat_id == 0. If the chat info database is not used, the chat_limit is ignored and is always set to 0
      chat_limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getStorageStatisticsFast
data GetStorageStatisticsFast
  = -- | Quickly returns approximate storage usage statistics. Can be called before authorization
  GetStorageStatisticsFast
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getDatabaseStatistics
data GetDatabaseStatistics
  = -- | Returns database statistics
  GetDatabaseStatistics
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function optimizeStorage
data OptimizeStorage
  = -- | Optimizes storage usage, i.e. deletes some files and returns new storage usage statistics. Secret thumbnails can't be deleted
  OptimizeStorage
    { -- | Limit on the total size of files after deletion. Pass -1 to use the default limit
      size :: I53,
      -- | Limit on the time that has passed since the last time a file was accessed (or creation time for some filesystems). Pass -1 to use the default limit
      ttl :: I32,
      -- | Limit on the total count of files after deletion. Pass -1 to use the default limit
      count :: I32,
      -- | The amount of time after the creation of a file during which it can't be deleted, in seconds. Pass -1 to use the default value
      immunity_delay :: I32,
      -- | If not empty, only files with the given type(s) are considered. By default, all types except thumbnails, profile photos, stickers and wallpapers are deleted
      file_types :: [FileType],
      -- | If not empty, only files from the given chats are considered. Use 0 as chat identifier to delete files not belonging to any chat (e.g., profile photos)
      chat_ids :: [I53],
      -- | If not empty, files from the given chats are excluded. Use 0 as chat identifier to exclude all files not belonging to any chat (e.g., profile photos)
      exclude_chat_ids :: [I53],
      -- | Pass true if deleted file statistics needs to be returned instead of the whole storage usage statistics. Affects only returned statistics
      return_deleted_file_statistics :: Bool,
      -- | Same as in getStorageStatistics. Affects only returned statistics
      chat_limit :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setNetworkType
data SetNetworkType
  = -- | Sets the current network type. Can be called before authorization. Calling this method forces all network connections to reopen, mitigating the delay in switching between different networks, so it should be called whenever the network is changed, even if the network type remains the same.
  SetNetworkType
    { type_ :: NetworkType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getNetworkStatistics
data GetNetworkStatistics
  = -- | Returns network data usage statistics. Can be called before authorization 
  GetNetworkStatistics
    { -- | If true, returns only data for the current library launch
      only_current :: Bool
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addNetworkStatistics
data AddNetworkStatistics
  = -- | Adds the specified data to data usage statistics. Can be called before authorization 
  AddNetworkStatistics
    { -- | The network statistics entry with the data to be added to statistics
      entry :: NetworkStatisticsEntry
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resetNetworkStatistics
data ResetNetworkStatistics
  = -- | Resets all network data usage statistics to zero. Can be called before authorization
  ResetNetworkStatistics
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getAutoDownloadSettingsPresets
data GetAutoDownloadSettingsPresets
  = -- | Returns auto-download settings presets for the current user
  GetAutoDownloadSettingsPresets
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setAutoDownloadSettings
data SetAutoDownloadSettings
  = -- | Sets auto-download settings 
  SetAutoDownloadSettings
    { -- | New user auto-download settings 
      settings :: AutoDownloadSettings,
      -- | Type of the network for which the new settings are applied
      type_ :: NetworkType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getBankCardInfo
data GetBankCardInfo
  = -- | Returns information about a bank card 
  GetBankCardInfo
    { -- | The bank card number
      bank_card_number :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPassportElement
data GetPassportElement
  = -- | Returns one of the available Telegram Passport elements 
  GetPassportElement
    { -- | Telegram Passport element type 
      type_ :: PassportElementType,
      -- | Password of the current user
      password :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getAllPassportElements
data GetAllPassportElements
  = -- | Returns all available Telegram Passport elements 
  GetAllPassportElements
    { -- | Password of the current user
      password :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setPassportElement
data SetPassportElement
  = -- | Adds an element to the user's Telegram Passport. May return an error with a message "PHONE_VERIFICATION_NEEDED" or "EMAIL_VERIFICATION_NEEDED" if the chosen phone number or the chosen email address must be verified first 
  SetPassportElement
    { -- | Input Telegram Passport element 
      element :: InputPassportElement,
      -- | Password of the current user
      password :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function deletePassportElement
data DeletePassportElement
  = -- | Deletes a Telegram Passport element 
  DeletePassportElement
    { -- | Element type
      type_ :: PassportElementType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setPassportElementErrors
data SetPassportElementErrors
  = -- | Informs the user that some of the elements in their Telegram Passport contain errors; for bots only. The user will not be able to resend the elements, until the errors are fixed 
  SetPassportElementErrors
    { -- | User identifier 
      user_id :: I32,
      -- | The errors
      errors :: [InputPassportElementError]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPreferredCountryLanguage
data GetPreferredCountryLanguage
  = -- | Returns an IETF language tag of the language preferred in the country, which should be used to fill native fields in Telegram Passport personal details. Returns a 404 error if unknown 
  GetPreferredCountryLanguage
    { -- | A two-letter ISO 3166-1 alpha-2 country code
      country_code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendPhoneNumberVerificationCode
data SendPhoneNumberVerificationCode
  = -- | Sends a code to verify a phone number to be added to a user's Telegram Passport
  SendPhoneNumberVerificationCode
    { -- | The phone number of the user, in international format 
      phone_number :: T,
      -- | Settings for the authentication of the user's phone number
      settings :: PhoneNumberAuthenticationSettings
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resendPhoneNumberVerificationCode
data ResendPhoneNumberVerificationCode
  = -- | Re-sends the code to verify a phone number to be added to a user's Telegram Passport
  ResendPhoneNumberVerificationCode
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkPhoneNumberVerificationCode
data CheckPhoneNumberVerificationCode
  = -- | Checks the phone number verification code for Telegram Passport 
  CheckPhoneNumberVerificationCode
    { -- | Verification code
      code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendEmailAddressVerificationCode
data SendEmailAddressVerificationCode
  = -- | Sends a code to verify an email address to be added to a user's Telegram Passport 
  SendEmailAddressVerificationCode
    { -- | Email address
      email_address :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resendEmailAddressVerificationCode
data ResendEmailAddressVerificationCode
  = -- | Re-sends the code to verify an email address to be added to a user's Telegram Passport
  ResendEmailAddressVerificationCode
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkEmailAddressVerificationCode
data CheckEmailAddressVerificationCode
  = -- | Checks the email address verification code for Telegram Passport 
  CheckEmailAddressVerificationCode
    { -- | Verification code
      code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPassportAuthorizationForm
data GetPassportAuthorizationForm
  = -- | Returns a Telegram Passport authorization form for sharing data with a service 
  GetPassportAuthorizationForm
    { -- | User identifier of the service's bot 
      bot_user_id :: I32,
      -- | Telegram Passport element types requested by the service 
      scope :: T,
      -- | Service's public_key 
      public_key :: T,
      -- | Authorization form nonce provided by the service
      nonce :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getPassportAuthorizationFormAvailableElements
data GetPassportAuthorizationFormAvailableElements
  = -- | Returns already available Telegram Passport elements suitable for completing a Telegram Passport authorization form. Result can be received only once for each authorization form 
  GetPassportAuthorizationFormAvailableElements
    { -- | Authorization form identifier 
      autorization_form_id :: I32,
      -- | Password of the current user
      password :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendPassportAuthorizationForm
data SendPassportAuthorizationForm
  = -- | Sends a Telegram Passport authorization form, effectively sharing data with the service. This method must be called after getPassportAuthorizationFormAvailableElements if some previously available elements need to be used
  SendPassportAuthorizationForm
    { -- | Authorization form identifier 
      autorization_form_id :: I32,
      -- | Types of Telegram Passport elements chosen by user to complete the authorization form
      types :: [PassportElementType]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendPhoneNumberConfirmationCode
data SendPhoneNumberConfirmationCode
  = -- | Sends phone number confirmation code. Should be called when user presses "https://t.me/confirmphone?phone=*******&hash=**********" or "tg://confirmphone?phone=*******&hash=**********" link 
  SendPhoneNumberConfirmationCode
    { -- | Value of the "hash" parameter from the link
      hash :: T,
      -- | Value of the "phone" parameter from the link 
      phone_number :: T,
      -- | Settings for the authentication of the user's phone number
      settings :: PhoneNumberAuthenticationSettings
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function resendPhoneNumberConfirmationCode
data ResendPhoneNumberConfirmationCode
  = -- | Resends phone number confirmation code
  ResendPhoneNumberConfirmationCode
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function checkPhoneNumberConfirmationCode
data CheckPhoneNumberConfirmationCode
  = -- | Checks phone number confirmation code 
  CheckPhoneNumberConfirmationCode
    { -- | The phone number confirmation code
      code :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setBotUpdatesStatus
data SetBotUpdatesStatus
  = -- | Informs the server about the number of pending bot updates if they haven't been processed for a long time; for bots only 
  SetBotUpdatesStatus
    { -- | The number of pending updates 
      pending_update_count :: I32,
      -- | The last error message
      error_message :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function uploadStickerFile
data UploadStickerFile
  = -- | Uploads a PNG image with a sticker; for bots only; returns the uploaded file
  UploadStickerFile
    { -- | Sticker file owner 
      user_id :: I32,
      -- | PNG image with the sticker; must be up to 512 KB in size and fit in 512x512 square
      png_sticker :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function createNewStickerSet
data CreateNewStickerSet
  = -- | Creates a new sticker set; for bots only. Returns the newly created sticker set
  CreateNewStickerSet
    { -- | Sticker set owner
      user_id :: I32,
      -- | Sticker set title; 1-64 characters
      title :: T,
      -- | Sticker set name. Can contain only English letters, digits and underscores. Must end with *"_by_<bot username>"* (*<bot_username>* is case insensitive); 1-64 characters
      name :: T,
      -- | True, if stickers are masks. Animated stickers can't be masks
      is_masks :: Bool,
      -- | List of stickers to be added to the set; must be non-empty. All stickers must be of the same type
      stickers :: [InputSticker]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addStickerToSet
data AddStickerToSet
  = -- | Adds a new sticker to a set; for bots only. Returns the sticker set
  AddStickerToSet
    { -- | Sticker set owner 
      user_id :: I32,
      -- | Sticker set name 
      name :: T,
      -- | Sticker to add to the set
      sticker :: InputSticker
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setStickerSetThumbnail
data SetStickerSetThumbnail
  = -- | Sets a sticker set thumbnail; for bots only. Returns the sticker set
  SetStickerSetThumbnail
    { -- | Sticker set owner 
      user_id :: I32,
      -- | Sticker set name
      name :: T,
      -- | Thumbnail to set in PNG or TGS format. Animated thumbnail must be set for animated sticker sets and only for them. You can use a zero InputFileId to delete the thumbnail
      thumbnail :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setStickerPositionInSet
data SetStickerPositionInSet
  = -- | Changes the position of a sticker in the set to which it belongs; for bots only. The sticker set must have been created by the bot
  SetStickerPositionInSet
    { -- | Sticker 
      sticker :: InputFile,
      -- | New position of the sticker in the set, zero-based
      position :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeStickerFromSet
data RemoveStickerFromSet
  = -- | Removes a sticker from the set to which it belongs; for bots only. The sticker set must have been created by the bot 
  RemoveStickerFromSet
    { -- | Sticker
      sticker :: InputFile
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getMapThumbnailFile
data GetMapThumbnailFile
  = -- | Returns information about a file with a map thumbnail in PNG format. Only map thumbnail files with size less than 1MB can be downloaded 
  GetMapThumbnailFile
    { -- | Location of the map center 
      location :: Location,
      -- | Map zoom level; 13-20 
      zoom :: I32,
      -- | Map width in pixels before applying scale; 16-1024 
      width :: I32,
      -- | Map height in pixels before applying scale; 16-1024 
      height :: I32,
      -- | Map scale; 1-3 
      scale :: I32,
      -- | Identifier of a chat, in which the thumbnail will be shown. Use 0 if unknown
      chat_id :: I53
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function acceptTermsOfService
data AcceptTermsOfService
  = -- | Accepts Telegram terms of services 
  AcceptTermsOfService
    { -- | Terms of service identifier
      terms_of_service_id :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function sendCustomRequest
data SendCustomRequest
  = -- | Sends a custom request; for bots only 
  SendCustomRequest
    { -- | The method name 
      method :: T,
      -- | JSON-serialized method parameters
      parameters :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function answerCustomQuery
data AnswerCustomQuery
  = -- | Answers a custom query; for bots only 
  AnswerCustomQuery
    { -- | Identifier of a custom query 
      custom_query_id :: I64,
      -- | JSON-serialized answer to the query
      data_ :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setAlarm
data SetAlarm
  = -- | Succeeds after a specified amount of time has passed. Can be called before authorization. Can be called before initialization 
  SetAlarm
    { -- | Number of seconds before the function returns
      seconds :: Double
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getCountryCode
data GetCountryCode
  = -- | Uses current user IP address to found their country. Returns two-letter ISO 3166-1 alpha-2 country code. Can be called before authorization
  GetCountryCode
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getInviteText
data GetInviteText
  = -- | Returns the default text for invitation messages to be used as a placeholder when the current user invites friends to Telegram
  GetInviteText
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getDeepLinkInfo
data GetDeepLinkInfo
  = -- | Returns information about a tg:// deep link. Use "tg://need_update_for_some_feature" or "tg:some_unsupported_feature" for testing. Returns a 404 error for unknown links. Can be called before authorization 
  GetDeepLinkInfo
    { -- | The link
      link :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getApplicationConfig
data GetApplicationConfig
  = -- | Returns application config, provided by the server. Can be called before authorization
  GetApplicationConfig
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function saveApplicationLogEvent
data SaveApplicationLogEvent
  = -- | Saves application log event on the server. Can be called before authorization 
  SaveApplicationLogEvent
    { -- | Event type 
      type_ :: T,
      -- | Optional chat identifier, associated with the event 
      chat_id :: I53,
      -- | The log event data
      data_ :: JsonValue
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addProxy
data AddProxy
  = -- | Adds a proxy server for network requests. Can be called before authorization 
  AddProxy
    { -- | Proxy server IP address 
      server :: T,
      -- | Proxy server port 
      port :: I32,
      -- | True, if the proxy should be enabled 
      enable :: Bool,
      -- | Proxy type
      type_ :: ProxyType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function editProxy
data EditProxy
  = -- | Edits an existing proxy server for network requests. Can be called before authorization 
  EditProxy
    { -- | Proxy identifier 
      proxy_id :: I32,
      -- | Proxy server IP address 
      server :: T,
      -- | Proxy server port 
      port :: I32,
      -- | True, if the proxy should be enabled 
      enable :: Bool,
      -- | Proxy type
      type_ :: ProxyType
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function enableProxy
data EnableProxy
  = -- | Enables a proxy. Only one proxy can be enabled at a time. Can be called before authorization 
  EnableProxy
    { -- | Proxy identifier
      proxy_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function disableProxy
data DisableProxy
  = -- | Disables the currently enabled proxy. Can be called before authorization
  DisableProxy
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function removeProxy
data RemoveProxy
  = -- | Removes a proxy server. Can be called before authorization 
  RemoveProxy
    { -- | Proxy identifier
      proxy_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getProxies
data GetProxies
  = -- | Returns list of proxies that are currently set up. Can be called before authorization
  GetProxies
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getProxyLink
data GetProxyLink
  = -- | Returns an HTTPS link, which can be used to add a proxy. Available only for SOCKS5 and MTProto proxies. Can be called before authorization 
  GetProxyLink
    { -- | Proxy identifier
      proxy_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function pingProxy
data PingProxy
  = -- | Computes time needed to receive a response from a Telegram server through a proxy. Can be called before authorization 
  PingProxy
    { -- | Proxy identifier. Use 0 to ping a Telegram server without a proxy
      proxy_id :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setLogStream
data SetLogStream
  = -- | Sets new log stream for internal logging of TDLib. This is an offline method. Can be called before authorization. Can be called synchronously 
  SetLogStream
    { -- | New log stream
      log_stream :: LogStream
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLogStream
data GetLogStream
  = -- | Returns information about currently used log stream for internal logging of TDLib. This is an offline method. Can be called before authorization. Can be called synchronously
  GetLogStream
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setLogVerbosityLevel
data SetLogVerbosityLevel
  = -- | Sets the verbosity level of the internal logging of TDLib. This is an offline method. Can be called before authorization. Can be called synchronously
  SetLogVerbosityLevel
    { -- | New value of the verbosity level for logging. Value 0 corresponds to fatal errors, value 1 corresponds to errors, value 2 corresponds to warnings and debug warnings, value 3 corresponds to informational, value 4 corresponds to debug, value 5 corresponds to verbose debug, value greater than 5 and up to 1023 can be used to enable even more logging
      new_verbosity_level :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLogVerbosityLevel
data GetLogVerbosityLevel
  = -- | Returns current verbosity level of the internal logging of TDLib. This is an offline method. Can be called before authorization. Can be called synchronously
  GetLogVerbosityLevel
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLogTags
data GetLogTags
  = -- | Returns list of available TDLib internal log tags, for example, ["actor", "binlog", "connections", "notifications", "proxy"]. This is an offline method. Can be called before authorization. Can be called synchronously
  GetLogTags
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function setLogTagVerbosityLevel
data SetLogTagVerbosityLevel
  = -- | Sets the verbosity level for a specified TDLib internal log tag. This is an offline method. Can be called before authorization. Can be called synchronously
  SetLogTagVerbosityLevel
    { -- | Logging tag to change verbosity level 
      tag :: T,
      -- | New verbosity level; 1-1024
      new_verbosity_level :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function getLogTagVerbosityLevel
data GetLogTagVerbosityLevel
  = -- | Returns current verbosity level for a specified TDLib internal log tag. This is an offline method. Can be called before authorization. Can be called synchronously 
  GetLogTagVerbosityLevel
    { -- | Logging tag to change verbosity level
      tag :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function addLogMessage
data AddLogMessage
  = -- | Adds a message to TDLib internal log. This is an offline method. Can be called before authorization. Can be called synchronously
  AddLogMessage
    { -- | The minimum verbosity level needed for the message to be logged, 0-1023 
      verbosity_level :: I32,
      -- | Text of a message to log
      text :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testCallEmpty
data TestCallEmpty
  = -- | Does nothing; for testing only. This is an offline method. Can be called before authorization
  TestCallEmpty
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testCallString
data TestCallString
  = -- | Returns the received string; for testing only. This is an offline method. Can be called before authorization 
  TestCallString
    { -- | String to return
      x :: T
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testCallBytes
data TestCallBytes
  = -- | Returns the received bytes; for testing only. This is an offline method. Can be called before authorization 
  TestCallBytes
    { -- | Bytes to return
      x :: ByteString64
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testCallVectorInt
data TestCallVectorInt
  = -- | Returns the received vector of numbers; for testing only. This is an offline method. Can be called before authorization 
  TestCallVectorInt
    { -- | Vector of numbers to return
      x :: [I32]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testCallVectorIntObject
data TestCallVectorIntObject
  = -- | Returns the received vector of objects containing a number; for testing only. This is an offline method. Can be called before authorization 
  TestCallVectorIntObject
    { -- | Vector of objects to return
      x :: [TestInt]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testCallVectorString
data TestCallVectorString
  = -- | Returns the received vector of strings; for testing only. This is an offline method. Can be called before authorization 
  TestCallVectorString
    { -- | Vector of strings to return
      x :: [T]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testCallVectorStringObject
data TestCallVectorStringObject
  = -- | Returns the received vector of objects containing a string; for testing only. This is an offline method. Can be called before authorization 
  TestCallVectorStringObject
    { -- | Vector of objects to return
      x :: [TestString]
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testSquareInt
data TestSquareInt
  = -- | Returns the squared received number; for testing only. This is an offline method. Can be called before authorization 
  TestSquareInt
    { -- | Number to square
      x :: I32
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testNetwork
data TestNetwork
  = -- | Sends a simple network request to the Telegram servers; for testing only. Can be called before authorization
  TestNetwork
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testProxy
data TestProxy
  = -- | Sends a simple network request to the Telegram servers via proxy; for testing only. Can be called before authorization 
  TestProxy
    { -- | Proxy server IP address 
      server :: T,
      -- | Proxy server port 
      port :: I32,
      -- | Proxy type
      type_ :: ProxyType,
      -- | Identifier of a datacenter, with which to test connection 
      dc_id :: I32,
      -- | The maximum overall timeout for the request
      timeout :: Double
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testGetDifference
data TestGetDifference
  = -- | Forces an updates.getDifference call to the Telegram servers; for testing only
  TestGetDifference
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testUseUpdate
data TestUseUpdate
  = -- | Does nothing and ensures that the Update object is used; for testing only. This is an offline method. Can be called before authorization
  TestUseUpdate
    { 
    }
  deriving (Show, Eq, Generic)
-- | Parameter of Function testReturnError
data TestReturnError
  = -- | Returns the specified error and ensures that the Error object is used; for testing only. This is an offline method. Can be called before authorization. Can be called synchronously 
  TestReturnError
    { -- | The error to be returned
      error :: Error
    }
  deriving (Show, Eq, Generic)

funArgInstances