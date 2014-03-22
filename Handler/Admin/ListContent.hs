{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.ListContent where

import Core.Import
import Yesod.Auth


class ListContent c where
    updateRoute :: c -> ContentId -> Route App
    viewRoute :: c -> ContentId -> Text -> Route App
    unpublishRoute :: c -> ContentId -> Route App
    publishRoute :: c -> ContentId -> Route App
    trashRoute :: c -> ContentId -> Route App
    msgContentSingle :: c -> AppMessage
    msgContentPlural :: c -> AppMessage
    msgNoContent :: c -> AppMessage

instance ListContent ContentKind where
    updateRoute Article = AdminUpdateArticleR
    updateRoute Page = AdminUpdateStaticPageR

    viewRoute Article = ArticleR
    viewRoute Page = StaticPageR

    unpublishRoute Article = AdminUnpublishArticleR
    unpublishRoute Page = AdminUnpublishStaticPageR

    publishRoute Article = AdminPublishArticleR
    publishRoute Page = AdminPublishStaticPageR

    trashRoute Article = AdminTrashArticleR
    trashRoute Page = AdminTrashStaticPageR

    msgContentSingle Article = MsgArticle
    msgContentSingle Page = MsgPage

    msgContentPlural Article = MsgArticles
    msgContentPlural Page = MsgPages

    msgNoContent Article = MsgNoArticles
    msgNoContent Page = MsgNoPages
