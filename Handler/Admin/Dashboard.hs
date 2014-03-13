{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Admin.Dashboard where

import Core.Import
import Yesod.Auth
import qualified Database.Esqueleto as E


articleCountByAuthor :: Handler [(Entity User, E.Value Int)]
articleCountByAuthor = runDB $ E.select $
    E.from $ \(c, u) -> do
    E.where_ (c E.^. ContentAuthor E.==. u E.^. UserId
        E.&&. c E.^. ContentType E.==. E.val Article
        E.&&. c E.^. ContentVisible E.==. E.val True
        E.&&. c E.^. ContentTrash E.==. E.val False)
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
