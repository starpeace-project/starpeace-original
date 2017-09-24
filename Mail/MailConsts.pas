unit MailConsts;

interface

  // Text constants

  const
    tidAccount_File   = 'account.ini';
    tidMessage_Header = 'msg.header';
    tidMessage_Body   = 'msg.body';
    tidAttchment_Mask = 'attach*.ini';

  const
    tidGeneral = 'General';
    tidAlias   = 'Alias';
    tidForward = 'Forward';
    tidAddress = 'Address';
    tidKeep    = 'Keep';

  const
    tidHeader      = 'Header';
    tidFromAddr    = 'FromAddr';
    tidToAddr      = 'ToAddr';
    tidFrom        = 'From';
    tidTo          = 'To';
    tidSubject     = 'Subject';
    tidMessageId   = 'MessageId';
    tidDate        = 'Date';
    tidDateFmt     = tidDate + 'Fmt';
    tidRead        = 'Read';
    tidAttachments = 'Attachments';
    tidClass       = 'Class';
    tidName        = 'Name';
    tidPriority    = 'Priority';
    tidStamp       = 'Stamp';
    tidNoReply     = 'NoReply';

  // Mail domains

  const
    tidTycoons   = 'tycoons';
    tidCompanies = 'companies';
    tidTowns     = 'towns';
    tidCluster   = 'clusters';

  // Mail Floders

  const
    tidInbox            = 'Inbox';
    tidSentItems        = 'Sent';
    tidDraft            = 'Draft';
    tidFolder_Inbox     = tidInbox;
    tidFolder_SentItems = tidSentItems;
    tidFolder_Draft     = tidDraft;

implementation

end.
