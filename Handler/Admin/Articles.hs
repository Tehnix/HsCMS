{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Articles (
      getAdminShowArticlesR
    , getAdminNewArticleR
    , postAdminNewArticleR
    , getAdminUpdateArticleR
    , postAdminUpdateArticleR
    , getAdminShowTrashArticlesR
    , postAdminTrashArticleR
    , postAdminUnpublishArticleR
    , postAdminPublishArticleR
  ) where

import           Core.Import
import           Handler.Admin.Content


-- | View all articles
getAdminShowArticlesR :: Handler Html
getAdminShowArticlesR = showContent Article MsgTitleAdminBlogArticles

-- | View all trashed articles
getAdminShowTrashArticlesR :: Handler Html
getAdminShowTrashArticlesR = showTrashContent Article MsgTitleAdminTrashArticles

-- | Mark an article as trashed
postAdminTrashArticleR :: ContentId -> Handler Html
postAdminTrashArticleR = postTrashContent MsgMsgDeletedArticle AdminShowArticlesR

-- | Change the status of an article to unpublished
postAdminUnpublishArticleR :: ContentId -> Handler Html
postAdminUnpublishArticleR = postUnpublishContent MsgMsgUnpublishedArticle AdminShowArticlesR

-- | Change the status of an article to published
postAdminPublishArticleR :: ContentId -> Handler Html
postAdminPublishArticleR = postPublishContent MsgMsgPublishedArticle AdminShowArticlesR

-- | Form page for creating a new article
getAdminNewArticleR :: Handler Html
getAdminNewArticleR = getNewContent Article MsgTitleAdminNewArticle AdminNewArticleR

-- | Handling the form for creating an article
postAdminNewArticleR :: Handler Html
postAdminNewArticleR = postNewContent Article MsgMsgCreatedArticleGistError MsgMsgCreatedArticle MsgMsgSavedArticle AdminUpdateArticleR AdminShowArticlesR

-- | Form page for updating an article
getAdminUpdateArticleR :: ContentId -> Handler Html
getAdminUpdateArticleR = getUpdateContent Article MsgTitleAdminUpdateArticle AdminUpdateArticleR

-- | Handling the form for updating an article
postAdminUpdateArticleR :: ContentId -> Handler Html
postAdminUpdateArticleR = postUpdateContent MsgMsgCreatedArticleGistError MsgMsgSavedArticle MsgMsgPublishedArticle MsgMsgUnpublishedArticle AdminUpdateArticleR AdminShowArticlesR
