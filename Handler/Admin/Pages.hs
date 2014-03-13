{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Pages (
      getAdminShowStaticPagesR
    , getAdminNewStaticPageR
    , postAdminNewStaticPageR
    , getAdminUpdateStaticPageR
    , postAdminUpdateStaticPageR
    , getAdminShowTrashStaticPagesR
    , postAdminTrashStaticPageR
    , postAdminUnpublishStaticPageR
    , postAdminPublishStaticPageR
  ) where

import           Core.Import
import           Handler.Admin.Content


-- | View all articles
getAdminShowStaticPagesR :: Handler Html
getAdminShowStaticPagesR = showContent Page MsgTitleAdminStaticPages

-- | View all trashed articles
getAdminShowTrashStaticPagesR :: Handler Html
getAdminShowTrashStaticPagesR = showTrashContent Page MsgTitleAdminTrashStaticPages

-- | Mark an article as trashed
postAdminTrashStaticPageR :: ContentId -> Handler Html
postAdminTrashStaticPageR = postTrashContent MsgMsgDeletedStaticPage AdminShowStaticPagesR

-- | Change the status of an article to unpublished
postAdminUnpublishStaticPageR :: ContentId -> Handler Html
postAdminUnpublishStaticPageR = postUnpublishContent MsgMsgUnpublishedStaticPage AdminShowStaticPagesR

-- | Change the status of an article to published
postAdminPublishStaticPageR :: ContentId -> Handler Html
postAdminPublishStaticPageR = postPublishContent MsgMsgPublishedStaticPage AdminShowStaticPagesR

-- | Form page for creating a new article
getAdminNewStaticPageR :: Handler Html
getAdminNewStaticPageR = getNewContent Page MsgTitleAdminNewStaticPage AdminNewStaticPageR

-- | Handling the form for creating an article
postAdminNewStaticPageR :: Handler Html
postAdminNewStaticPageR = postNewContent Page MsgMsgCreatedStaticPageGistError MsgMsgCreatedStaticPage MsgMsgSavedStaticPage AdminUpdateStaticPageR AdminShowStaticPagesR

-- | Form page for updating an article
getAdminUpdateStaticPageR :: ContentId -> Handler Html
getAdminUpdateStaticPageR = getUpdateContent Page MsgTitleAdminUpdateStaticPage AdminUpdateStaticPageR

-- | Handling the form for updating an article
postAdminUpdateStaticPageR :: ContentId -> Handler Html
postAdminUpdateStaticPageR = postUpdateContent MsgMsgCreatedStaticPageGistError MsgMsgSavedStaticPage MsgMsgPublishedStaticPage MsgMsgUnpublishedStaticPage AdminUpdateStaticPageR AdminShowStaticPagesR
