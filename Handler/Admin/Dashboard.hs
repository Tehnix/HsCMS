{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Dashboard where

import Core.Import
import Yesod.Auth
import qualified Database.Esqueleto as E


articleCountByAuthor :: Handler [(Entity User, E.Value Int)]
articleCountByAuthor = runDB $ E.select $
    E.from $ \(a, u) -> do
    E.where_ (a E.^. ArticleAuthor E.==. u E.^. UserId
        E.&&. a E.^. ArticleVisible E.==. E.val True
        E.&&. a E.^. ArticleTrash E.==. E.val False)
    E.groupBy (u E.^. UserId)
    let cnt = E.countRows :: E.SqlExpr (E.Value Int)
    E.orderBy [E.asc (u E.^. UserIdent)]
    return (u, cnt)

getAdminDashboardR :: Handler Html
getAdminDashboardR = do
    userEmail <- fmap usersEmail maybeAuth
    postCount <- articleCountByAuthor
    adminLayout $ do
        setTitleI MsgTitleAdminDashboard
        toWidget [lucius| #navigation .navigation-dashboard { background: red; } |]
        addScript $ StaticR js_chart_js
        $(widgetFile "admin/dashboard")
